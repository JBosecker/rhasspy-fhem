##############################################
#
# FHEM Rhasspy module  https://github.com/rhasspy/rhasspy)
#
# written 2018 by Tobias Wiedenmann (Thyraz)
# forked and adopted for Rhasspy 2020 by Johannes Bosecker (JBosecker)
# thanks to Matthias Kleine
#
##############################################

use strict;
use warnings;

my %gets = (
    "version" => "",
    "status" => ""
);

my %sets = (
    "say" => "",
    "play" => "",
    "updateModel" => "",
    "textCommand" => "",
    "volume" => "",
    "reconnect" => ""
);

# MQTT Topics die das Modul automatisch abonniert
my @topics = qw(
    hermes/intent/+
    hermes/nlu/intentNotRecognized
    hermes/hotword/toggleOff
    hermes/hotword/toggleOn
);


sub RHASSPY_Initialize($) {
    my $hash = shift @_;

    # Attribute rhasspyName und rhasspyRoom für andere Devices zur Verfügung stellen
    addToAttrList("rhasspyName");
    addToAttrList("rhasspyRoom");
    addToAttrList("rhasspyMapping:textField-long");

    # Consumer
    $hash->{DefFn} = "RHASSPY::Define";
    $hash->{UndefFn} = "RHASSPY::Undefine";
    $hash->{SetFn} = "RHASSPY::Set";
    $hash->{AttrFn} = "RHASSPY::Attr";
    $hash->{AttrList} = "disable:0,1 defaultRoom rhasspyIntents:textField-long shortcuts:textField-long response:textField-long " . $main::readingFnAttributes;
    $hash->{ReadFn}   = "RHASSPY::ioDevRead";
    $hash->{ReadyFn}  = "RHASSPY::ioDevReady";
}

# Cmd in main:: ausführen damit User den Prefix nicht vor alle Perl-Aufrufe schreiben muss
sub RHASSPY_execute($$$$$) {
    my ($hash, $device, $cmd, $value, $siteId) = @_;
    my $returnVal;

    # Nutervariablen setzen
    my $DEVICE = $device;
    my $VALUE = $value;
    my $ROOM = (defined($siteId) && $siteId eq "default") ? $hash->{helper}{defaultRoom} : $siteId;

    # CMD ausführen
    $returnVal = eval $cmd;
    Log3($hash->{NAME}, 1, $@) if ($@);

    return $returnVal;
}


package RHASSPY;

use strict;
use warnings;
use POSIX;
use GPUtils qw(:all);
use JSON;
use Net::MQTT::Constants;
use Encode;
use Time::HiRes qw(gettimeofday);
use JSON qw(decode_json encode_json);
use MIME::Base64;
use Encode qw(encode_utf8 decode_utf8);
use HttpUtils;
use DevIo;
use Digest::SHA qw(sha1_hex);
use utf8;

# use Data::Dumper 'Dumper';

BEGIN {
    GP_Import(qw(
        devspec2array
        CommandDeleteReading
        CommandAttr
        readingsSingleUpdate
        readingsBulkUpdate
        readingsBeginUpdate
        readingsEndUpdate
        Log3
        fhem
        defs
        AttrVal
        ReadingsVal
        round
        toJSON
        AnalyzeCommand
        AnalyzeCommandChain
        AnalyzePerlCommand
        parseParams
        looks_like_number
        EvalSpecials
        FmtDateTimeRFC1123
        DevIo_OpenDev
        DevIo_CloseDev
        DevIo_SimpleRead
        DevIo_SimpleWrite
        HttpUtils_NonblockingGet
        trim
        InternalTimer
        RemoveInternalTimer
        urlEncode
    ))
};


my %websocketOpcode = (    # Opcode interpretation of the ws "Payload data
  'continuation'  => 0x00,
  'text'          => 0x01,
  'binary'        => 0x02,
  'close'         => 0x08,
  'ping'          => 0x09,
  'pong'          => 0x0A
);


# Device anlegen
sub Define() {
    my ($hash, $def) = @_;
    my @args = split("[ \t]+", $def);

    # Minimale Anzahl der nötigen Argumente vorhanden?
    return "Invalid number of arguments: define <name> RHASSPY Host Port DefaultRoom" if (int(@args) < 5);

    my ($name, $type, $host, $port, $defaultRoom) = @args;
    $hash->{MODULE_VERSION} = "0.1";
    $hash->{helper}{defaultRoom} = $defaultRoom;

    # HOST und PORT setzen
    $hash->{HOST} = $host;
    $hash->{PORT} = $port;

    # DeviceName für IoDev setzen
    $hash->{DeviceName} = "$host:$port";

    RHASSPY::ioDevReconnect($hash);
};


# Device löschen
sub Undefine($$) {
    my ($hash, $name) = @_;

    DevIo_CloseDev($hash);
    return undef;
}


# Set Befehl aufgerufen
sub Set($$$@) {
    my ($hash, $name, $command, @values) = @_;
    return "Unknown argument $command, choose one of " . join(" ", sort keys %sets) if(!defined($sets{$command}));

    my $text = decode_utf8(join(" ", @values));
    Log3($hash->{NAME}, 5, "set " . $command . " - value: " . $text);

    # Say Cmd
    if ($command eq "say") {
        RHASSPY::say($hash, $text);
    }
    elsif ($command eq "play") {
        RHASSPY::playBytes($hash, $text);
    }
    # TextCommand Cmd
    elsif ($command eq "textCommand") {
        RHASSPY::textCommand($hash, $text);
    }
    # Update Model Cmd
    elsif ($command eq "updateModel") {
        RHASSPY::updateModel($hash);
    }
    # Volume Cmd
    elsif ($command eq "volume") {
        RHASSPY::setVolume($hash, $text);
    }
    # Reconnect
    elsif ($command eq "reconnect") {
        RHASSPY::ioDevReconnect($hash);
    }
}


# Attribute setzen / löschen
sub Attr($$$$) {
    my ($command, $name, $attribute, $value) = @_;
    my $hash = $defs{$name};

    # IODev Attribut gesetzt
    if ($attribute eq "disable") {
        RHASSPY::ioDevReconnect($hash);
    }

    return undef;
}


# Alle Gerätenamen sammeln
sub allRhasspyNames() {
    my @devices, my @sorted;
    my %devicesHash;
    my $devspec = "room=Rhasspy";
    my @devs = devspec2array($devspec);

    # Alle RhasspyNames sammeln
    foreach (@devs) {
        push @devices, split(',', decode_utf8(AttrVal($_,"rhasspyName",undef)));
    }

    # Doubletten rausfiltern
    %devicesHash = map { if (defined($_)) { $_, 1 } else { () } } @devices;
    @devices = keys %devicesHash;

    # Längere Werte zuerst, damit bei Ersetzungen z.B. nicht 'lampe' gefunden wird bevor der eigentliche Treffer 'deckenlampe' versucht wurde
    @sorted = sort { length($b) <=> length($a) } @devices;

    return @sorted
}


# Alle Raumbezeichnungen sammeln
sub allRhasspyRooms() {
    my @rooms, my @sorted;
    my %roomsHash;
    my $devspec = "room=Rhasspy";
    my @devs = devspec2array($devspec);

    # Alle RhasspyNames sammeln
    foreach (@devs) {
        push @rooms, split(',', decode_utf8(AttrVal($_,"rhasspyRoom",undef)));
    }

    # Doubletten rausfiltern
    %roomsHash = map { if (defined($_)) { $_, 1 } else { () } } @rooms;
    @rooms = keys %roomsHash;

    # Längere Werte zuerst, damit bei Ersetzungen z.B. nicht 'küche' gefunden wird bevor der eigentliche Treffer 'waschküche' versucht wurde
    @sorted = sort { length($b) <=> length($a) } @rooms;

    return @sorted
}


# Alle Sender sammeln
sub allRhasspyChannels() {
    my @channels, my @sorted;
    my %channelsHash;
    my $devspec = "room=Rhasspy";
    my @devs = devspec2array($devspec);

    # Alle RhasspyNames sammeln
    foreach (@devs) {
        my @rows = split(/\n/, decode_utf8(AttrVal($_,"rhasspyChannels",undef)));
        foreach (@rows) {
            my @tokens = split('=', $_);
            my $channel = shift(@tokens);
            push @channels, $channel;
        }
    }

    # Doubletten rausfiltern
    %channelsHash = map { if (defined($_)) { $_, 1 } else { () } } @channels;
    @channels = keys %channelsHash;

    # Längere Werte zuerst, damit bei Ersetzungen z.B. nicht 'S.W.R.' gefunden wird bevor der eigentliche Treffer 'S.W.R.3' versucht wurde
    @sorted = sort { length($b) <=> length($a) } @channels;

    return @sorted
}


# Alle NumericTypes sammeln
sub allRhasspyTypes() {
    my @types, my @sorted;
    my %typesHash;
    my $devspec = "room=Rhasspy";
    my @devs = devspec2array($devspec);

    # Alle RhasspyNames sammeln
    foreach (@devs) {
        my @mappings = split(/\n/, decode_utf8(AttrVal($_,"rhasspyMapping",undef)));
        foreach (@mappings) {
            # Nur GetNumeric und SetNumeric verwenden
            next unless $_ =~ m/^(SetNumeric|GetNumeric)/;
            $_ =~ s/(SetNumeric|GetNumeric)://;
            my %mapping = splitMappingString($_);

            push @types, $mapping{'type'} if (defined($mapping{'type'}));
        }
    }

    # Doubletten rausfiltern
    %typesHash = map { if (defined($_)) { $_, 1 } else { () } } @types;
    @types = keys %typesHash;

    # Längere Werte zuerst, damit bei Ersetzungen z.B. nicht 'S.W.R.' gefunden wird bevor der eigentliche Treffer 'S.W.R.3' versucht wurde
    @sorted = sort { length($b) <=> length($a) } @types;

    return @sorted
}


# Alle Farben sammeln
sub allRhasspyColors() {
    my @colors, my @sorted;
    my %colorHash;
    my $devspec = "room=Rhasspy";
    my @devs = devspec2array($devspec);

    # Alle RhasspyNames sammeln
    foreach (@devs) {
        my @rows = split(/\n/, decode_utf8(AttrVal($_,"rhasspyColors",undef)));
        foreach (@rows) {
            my @tokens = split('=', $_);
            my $color = shift(@tokens);
            push @colors, $color;
        }
    }

    # Doubletten rausfiltern
    %colorHash = map { if (defined($_)) { $_, 1 } else { () } } @colors;
    @colors = keys %colorHash;

    # Längere Werte zuerst, damit bei Ersetzungen z.B. nicht 'S.W.R.' gefunden wird bevor der eigentliche Treffer 'S.W.R.3' versucht wurde
    @sorted = sort { length($b) <=> length($a) } @colors;

    return @sorted
}


# Alle Shortcuts sammeln
sub allRhasspyShortcuts($) {
    my ($hash) = @_;
    my @shortcuts, my @sorted;
    my @rows = split(/\n/, decode_utf8(AttrVal($hash->{NAME}, "shortcuts", "")));
    foreach (@rows) {
        my @tokens = split('=', $_);
        my $shortcut = shift(@tokens);
        push @shortcuts, $shortcut;
    }

    # Längere Werte zuerst, damit bei Ersetzungen z.B. nicht 'S.W.R.' gefunden wird bevor der eigentliche Treffer 'S.W.R.3' versucht wurde
    @sorted = sort { length($b) <=> length($a) } @shortcuts;

    return @sorted
}


# Alle Sentences aus Datei lesen
sub allRhasspySentences() {
    my $modPath = AttrVal("global", "modpath", ".");
    my $fileName = "rhasspy_sentences.ini";
    my $filePath = "$modPath/FHEM/$fileName";

    my $document = do {
        local $/ = undef;
        open my $fh, "<:encoding(UTF-8)", $filePath;
        <$fh>;
    };

    return $document;
}


# Alle Artikel sammeln
sub allRhasspyArticles() {
    my @articles;
    push @articles, "der";
    push @articles, "die";
    push @articles, "das";
    push @articles, "den";

    return @articles;
}


# Alle Präpositionen für Räume sammeln
sub allRhasspyRoomPrepositions() {
    my @prepositions;
    push @prepositions, "in der";
    push @prepositions, "in dem";
    push @prepositions, "im";
    push @prepositions, "auf der";
    push @prepositions, "auf dem";


    return @prepositions;
}


# Alle Präpositionen für Geräte sammeln
sub allRhasspyDevicePrepositions() {
    my @prepositions;
    push @prepositions, "vom";
    push @prepositions, "von";
    push @prepositions, "von dem";
    push @prepositions, "von der";
    push @prepositions, "des";
    push @prepositions, "im";

    return @prepositions;
}


# Raum aus gesprochenem Text oder aus siteId verwenden? (siteId "default" durch Attr defaultRoom ersetzen)
sub roomName ($$) {
    my ($hash, $data) = @_;

    my $room;
    my $defaultRoom = $hash->{helper}{defaultRoom};

    # Slot "Room" im JSON vorhanden? Sonst Raum des angesprochenen Satelites verwenden
    if (exists($data->{'Room'})) {
        $room = $data->{'Room'};
    } else {
        $room = $data->{'siteId'};
        $room = $defaultRoom if ($room eq 'default' || !(length $room));
    }

    return $room;
}


# Gerät über Raum und Namen suchen.
sub getDeviceByName($$$) {
    my ($hash, $room, $name) = @_;
    my $device;
    my $devspec = "room=Rhasspy";
    my @devices = devspec2array($devspec);

    # devspec2array sendet bei keinen Treffern als einziges Ergebnis den devSpec String zurück
    return undef if (@devices == 1 && $devices[0] eq $devspec);

    foreach (@devices) {
        # 2 Arrays bilden mit Namen und Räumen des Devices
        my @names = split(',', decode_utf8(AttrVal($_,"rhasspyName",undef)));
        my @rooms = split(',', decode_utf8(AttrVal($_,"rhasspyRoom",undef)));

        # Case Insensitive schauen ob der gesuchte Name (oder besser Name und Raum) in den Arrays vorhanden ist
        if (grep( /^$name$/i, @names)) {
            if (!defined($device) || grep( /^$room$/i, @rooms)) {
                $device = $_;
            }
        }
    }

    Log3($hash->{NAME}, 5, "Device selected: $device");

    return $device;
}


# Sammelt Geräte über Raum, Intent und optional Type
sub getDevicesByIntentAndType($$$$) {
    my ($hash, $room, $intent, $type) = @_;
    my @matchesInRoom, my @matchesOutsideRoom;
    my $devspec = "room=Rhasspy";
    my @devices = devspec2array($devspec);

    # devspec2array sendet bei keinen Treffern als einziges Ergebnis den devSpec String zurück
    return undef if (@devices == 1 && $devices[0] eq $devspec);

    foreach (@devices) {
        # Array bilden mit Räumen des Devices
        my @rooms = split(',', decode_utf8(AttrVal($_,"rhasspyRoom",undef)));
        # Mapping mit passendem Intent vorhanden?
        my $mapping = RHASSPY::getMapping($hash, $_, $intent, $type, 1);
        next unless defined($mapping);

        my $mappingType = $mapping->{'type'} if (defined($mapping->{'type'}));

        # Geräte sammeln
        if (!defined($type) && !(grep(/^$room$/i, @rooms))) {
            push @matchesOutsideRoom, $_;
        }
        elsif (!defined($type) && grep(/^$room$/i, @rooms)) {
            push @matchesInRoom, $_;
        }
        elsif (defined($type) && $type =~ m/^$mappingType$/i && !(grep(/^$room$/i, @rooms))) {
            push @matchesOutsideRoom, $_;
        }
        elsif (defined($type) && $type =~ m/^$mappingType$/i && grep(/^$room$/i, @rooms)) {
            push @matchesInRoom, $_;
        }
    }

    return (\@matchesInRoom, \@matchesOutsideRoom);
}


# Geräte über Raum, Intent und ggf. Type suchen.
sub getDeviceByIntentAndType($$$$) {
    my ($hash, $room, $intent, $type) = @_;
    my $device;

    # Devices sammeln
    my ($matchesInRoom, $matchesOutsideRoom) = getDevicesByIntentAndType($hash, $room, $intent, $type);

    # Erstes Device im passenden Raum zurückliefern falls vorhanden, sonst erstes Device außerhalb
    $device = (@{$matchesInRoom} > 0) ? shift @{$matchesInRoom} : shift @{$matchesOutsideRoom};

    Log3($hash->{NAME}, 5, "Device selected: $device");

    return $device;
}


# Eingeschaltetes Gerät mit bestimmten Intent und optional Type suchen
sub getActiveDeviceForIntentAndType($$$$) {
    my ($hash, $room, $intent, $type) = @_;
    my $device;
    my ($matchesInRoom, $matchesOutsideRoom) = getDevicesByIntentAndType($hash, $room, $intent, $type);

    # Anonyme Funktion zum finden des aktiven Geräts
    my $activeDevice = sub ($$) {
        my ($hash, $devices) = @_;
        my $match;

        foreach (@{$devices}) {
            my $mapping = getMapping($hash, $_, "GetOnOff", undef, 1);
            if (defined($mapping)) {
                # Gerät ein- oder ausgeschaltet?
                my $value = getOnOffState($hash, $_, $mapping);
                if ($value == 1) {
                    $match = $_;
                    last;
                }
            }
        }
        return $match;
    };

    # Gerät finden, erst im aktuellen Raum, sonst in den restlichen
    $device = $activeDevice->($hash, $matchesInRoom);
    $device = $activeDevice->($hash, $matchesOutsideRoom) if (!defined($device));

    Log3($hash->{NAME}, 5, "Device selected: $device");

    return $device;
}


# Gerät mit bestimmtem Sender suchen
sub getDeviceByMediaChannel($$$) {
    my ($hash, $room, $channel) = @_;
    my $device;
    my $devspec = "room=Rhasspy";
    my @devices = devspec2array($devspec);

    # devspec2array sendet bei keinen Treffern als einziges Ergebnis den devSpec String zurück
    return undef if (@devices == 1 && $devices[0] eq $devspec);

    foreach (@devices) {
        # Array bilden mit Räumen des Devices
        my @rooms = split(',', decode_utf8(AttrVal($_,"rhasspyRoom",undef)));
        # Cmd mit passendem Intent vorhanden?
        my $cmd = getCmd($hash, $_, "rhasspyChannels", $channel, 1);
        next unless defined($cmd);

        # Erster Treffer wälen, überschreiben falls besserer Treffer (Raum matched auch) kommt
        if (!defined($device) || grep(/^$room$/i, @rooms)) {
            $device = $_;
        }
    }

    Log3($hash->{NAME}, 5, "Device selected: $device");

    return $device;
}


# Mappings in Key/Value Paare aufteilen
sub splitMappingString($) {
    my ($mapping) = @_;
    my @tokens, my $token = '';
    my $char, my $lastChar = '';
    my $bracketLevel = 0;
    my %parsedMapping;

    # String in Kommagetrennte Tokens teilen
    foreach $char (split(//, $mapping)) {
        if ($char eq '{' && $lastChar ne '\\') {
            $bracketLevel += 1;
            $token .= $char;
        }
        elsif ($char eq '}' && $lastChar ne '\\') {
            $bracketLevel -= 1;
            $token .= $char;
        }
        elsif ($char eq ',' && $lastChar ne '\\' && $bracketLevel == 0) {
            push(@tokens, $token);
            $token = '';
        }
        else {
            $token .= $char;
        }

        $lastChar = $char;
    }
    push(@tokens, $token) if (length($token) > 0);

    # Tokens in Keys/Values trennen
    %parsedMapping = map {split /=/, $_, 2} @tokens;

    return %parsedMapping;
}


# rhasspyMapping parsen und gefundene Settings zurückliefern
sub getMapping($$$$;$) {
    my ($hash, $device, $intent, $type, $disableLog) = @_;
    my @mappings, my $matchedMapping;
    my $mappingsString = decode_utf8(AttrVal($device, "rhasspyMapping", undef));

    if (defined($mappingsString)) {
        # String in einzelne Mappings teilen
        @mappings = split(/\n/, $mappingsString);

        foreach (@mappings) {
            # Nur Mappings vom gesuchten Typ verwenden
            next unless $_ =~ qr/^$intent/;
            $_ =~ s/$intent://;
            my %currentMapping = splitMappingString($_);

            # Erstes Mapping vom passenden Intent wählen (unabhängig vom Type), dann ggf. weitersuchen ob noch ein besserer Treffer mit passendem Type kommt
            if (!defined($matchedMapping) || (defined($type) && lc($matchedMapping->{'type'}) ne lc($type) && lc($currentMapping{'type'}) eq lc($type))) {
                $matchedMapping = \%currentMapping;

                Log3($hash->{NAME}, 5, "rhasspyMapping selected: $_") if (!defined($disableLog) || (defined($disableLog) && $disableLog != 1));
            }
        }
    }
    return $matchedMapping;
}


# Cmd von Attribut mit dem Format value=cmd pro Zeile lesen
sub getCmd($$$$;$) {
    my ($hash, $device, $reading, $key, $disableLog) = @_;
    my @rows, my $cmd;
    my $attrString = decode_utf8(AttrVal($device, $reading, undef));

    # String in einzelne Mappings teilen
    @rows = split(/\n/, $attrString);

    foreach (@rows) {
        # Nur Zeilen mit gesuchten Identifier verwenden
        next unless $_ =~ qr/^$key=/i;
        $_ =~ s/$key=//i;
        $cmd = $_;

        Log3($hash->{NAME}, 5, "cmd selected: $_") if (!defined($disableLog) || (defined($disableLog) && $disableLog != 1));
        last;
    }

    return $cmd;
}


# Cmd String im Format 'cmd', 'device:cmd', 'fhemcmd1; fhemcmd2' oder '{<perlcode}' ausführen
sub runCmd($$$;$$) {
    my ($hash, $device, $cmd, $val, $siteId) = @_;
    my $error;
    my $returnVal;

    # Perl Command
    if ($cmd =~ m/^\s*{.*}\s*$/) {
        # CMD ausführen
        $returnVal = main::RHASSPY_execute($hash, $device, $cmd, $val, $siteId);
    }
    # String in Anführungszeichen (mit ReplaceSetMagic)
    elsif ($cmd =~ m/^\s*".*"\s*$/) {
        my $DEVICE = $device;
        my $ROOM = $siteId;
        my $VALUE = $val;

        # Anführungszeichen entfernen
        $cmd =~ s/^\s*"//;
        $cmd =~ s/"\s*$//;

        # Variablen ersetzen?
        eval { $cmd =~ s/(\$\w+)/$1/eeg; };

        # [DEVICE:READING] Einträge erstzen
        $returnVal = ReplaceReadingsVal($hash, $cmd);
        # Escapte Kommas wieder durch normale ersetzen
        $returnVal =~ s/\\,/,/;
    }
    # FHEM Command oder CommandChain
    elsif (defined($main::cmds{ (split " ", $cmd)[0] })) {
        $error = AnalyzeCommandChain($hash, $cmd);
    }
    # Soll Command auf anderes Device umgelenkt werden?
    elsif ($cmd =~ m/:/) {
        $cmd =~ s/:/ /;
        $cmd = $cmd . ' ' . $val if (defined($val));
        $error = AnalyzeCommand($hash, "set $cmd");
    }
    # Nur normales Cmd angegeben
    else {
        $cmd = "$device $cmd";
        $cmd = $cmd . ' ' . $val if (defined($val));
        $error = AnalyzeCommand($hash, "set $cmd");
    }
    Log3($hash->{NAME}, 1, $_) if (defined($error));

    return $returnVal;
}


# Wert über Format 'reading', 'device:reading' oder '{<perlcode}' lesen
sub getValue($$$;$$) {
    my ($hash, $device, $getString, $val, $siteId) = @_;
    my $value;

    # Perl Command? -> Umleiten zu runCmd
    if ($getString =~ m/^\s*{.*}\s*$/) {
        # Wert lesen
        $value = runCmd($hash, $device, $getString, $val, $siteId);
    }
    # String in Anführungszeichen -> Umleiten zu runCmd
    elsif ($getString =~ m/^\s*".*"\s*$/) {
        # Wert lesen
        $value = runCmd($hash, $device, $getString, $val, $siteId);
    }
    # Reading oder Device:Reading
    else {
      # Soll Reading von einem anderen Device gelesen werden?
      my $readingsDev = ($getString =~ m/:/) ? (split(/:/, $getString))[0] : $device;
      my $reading = ($getString =~ m/:/) ? (split(/:/, $getString))[1] : $getString;

      $value = ReadingsVal($readingsDev, $reading, 0);
    }

    return $value;
}


# Zustand eines Gerätes über GetOnOff Mapping abfragen
sub getOnOffState ($$$) {
    my ($hash, $device, $mapping) = @_;
    my $valueOn   = (defined($mapping->{'valueOn'}))  ? $mapping->{'valueOn'}  : undef;
    my $valueOff  = (defined($mapping->{'valueOff'})) ? $mapping->{'valueOff'} : undef;
    my $value = getValue($hash, $device, $mapping->{'currentVal'});

    # Entscheiden ob $value 0 oder 1 ist
    if (defined($valueOff)) {
        $value = (lc($value) eq lc($valueOff)) ? 0 : 1;
    } elsif (defined($valueOn)) {
        $value = (lc($value) eq lc($valueOn)) ? 1 : 0;
    } else {
        # valueOn und valueOff sind nicht angegeben worden, alles außer "off" wird als eine 1 gewertet
        $value = (lc($value) eq "off") ? 0 : 1;
    }

    return $value;
}


# Umlaute durch "normale" Buchstaben austauschen
sub replaceUmlauts {
  my ($string) = @_;

  my %umlauts = ("ä" => "ae", "Ä" => "Ae", "ü" => "ue", "Ü" => "Ue", "ö" => "oe", "Ö" => "Oe", "ß" => "ss" );
  my $keys = join("|", keys(%umlauts));

  my $fixedString = decode_utf8($string);
  $fixedString =~ s/($keys)/$umlauts{$1}/g;
  return encode_utf8($fixedString);
}


# JSON parsen
sub parseJSON($$) {
    my ($hash, $json) = @_;
    my $data;

    # JSON Decode und Fehlerüberprüfung
    my $decoded = eval { decode_json(encode_utf8($json)) };
    if ($@) {
        Log3($hash->{NAME}, 1, "JSON decoding error: " . $@);
        return undef;
    }

    # Standard-Keys auslesen
    if (exists($decoded->{'intent'})) {
        ($data->{'intent'} = $decoded->{'intent'}{'intentName'}) =~ s/^.*.://;
    }

    $data->{'probability'} = $decoded->{'intent'}{'confidenceScore'};
    $data->{'sessionId'} = $decoded->{'sessionId'};
    $data->{'siteId'} = $decoded->{'siteId'};
    $data->{'input'} = $decoded->{'input'};

    # Überprüfen ob Slot Array existiert
    if (exists($decoded->{'slots'})) {
        my @slots = @{$decoded->{'slots'}};

        # Key -> Value Paare aus dem Slot Array ziehen
        foreach my $slot (@slots) {
            my $slotName = $slot->{'slotName'};
            my $slotValue;

            $slotValue = $slot->{'value'}{'value'} if (exists($slot->{'value'}) && exists($slot->{'value'}{'value'}));
            $slotValue = $slot->{'value'} if (exists($slot->{'entity'}) && $slot->{'entity'} eq "rhasspy/duration");

            $data->{$slotName} = $slotValue;
        }
    }

    # Falls Info Dict angehängt ist, handelt es sich um einen mit Standardwerten über NLU umgeleiteten Request. -> Originalwerte wiederherstellen
    if (exists($decoded->{'id'})) {
        my $info = eval { decode_json($decoded->{'id'}) };
        if ($@) {
            $info = undef;
        }

        $data->{'input'} = $info->{'input'} if defined($info->{'input'});
        $data->{'sessionId'} = $info->{'sessionId'} if defined($info->{'sessionId'});
        $data->{'siteId'} = $info->{'siteId'} if defined($info->{'siteId'});
        $data->{'Device'} = $info->{'Device'} if defined($info->{'Device'});
        $data->{'Room'} = $info->{'Room'} if defined($info->{'Room'});
        $data->{'Channel'} = $info->{'Channel'} if defined($info->{'Channel'});
        $data->{'Color'} = $info->{'Color'} if defined($info->{'Color'});
        $data->{'Type'} = $info->{'Type'} if defined($info->{'Type'});
    }

    my $result;

    foreach (keys %{ $data }) {
        my $value = $data->{$_};
        my $decodedKey = decode_utf8($_);
        my $decodedValue = decode_utf8($value);
        my $logValue = defined($decodedValue) ? $decodedValue : "";

        $result->{$decodedKey} = $decodedValue;
        Log3($hash->{NAME}, 5, "Parsed value: $logValue (hex: " . unpack('H*', $logValue) . ") for key: $decodedKey");
    }

    return $result;
}

# Daten vom MQTT Modul empfangen -> Device und Room ersetzen, dann erneut an NLU übergeben
sub onmessage($$$) {
    my ($hash, $topic, $message) = @_;
    my $data = RHASSPY::parseJSON($hash, $message);
    my $input = $data->{'input'} if defined($data->{'input'});

    # Hotword Erkennung
    if ($topic =~ m/^hermes\/hotword/) {
        my $room = roomName($hash, $data);

        if (defined($room)) {
            # We need to delay the resetting of the listening readings because of multiple calls
            # Otherwise it would be 1 ---- 0 - 1 ---- 0 - 1 ---- 0 in a short period of time
            RHASSPY::removeIdentifiableInternalTimer($room, $hash, "RHASSPY::setListeningOff");

            if ($topic =~ m/toggleOff/) {
                RHASSPY::setListeningOn($hash, $room);
            } elsif ($topic =~ m/toggleOn/) {
                RHASSPY::identifiableInternalTimer($room, gettimeofday() + 2, "RHASSPY::setListeningOff", $hash, 0)
            }
        }
    }

    # Shortcut empfangen -> Code direkt ausführen
    elsif ($topic =~ qr/^hermes\/intent\/.*/ && defined($input) && grep( /^$input$/i, allRhasspyShortcuts($hash))) {
      my $error;
      my $response = getResponse($hash, "DefaultError");
      my $type      = ($topic eq "hermes/intent/FHEM:TextCommand") ? "text" : "voice";
      my $sessionId = ($topic eq "hermes/intent/FHEM:TextCommand") ? ""     : $data->{'sessionId'};
      my $cmd = getCmd($hash, $hash->{NAME}, "shortcuts", $input);

      if (defined($cmd)) {
          # Cmd ausführen
          my $returnVal = runCmd($hash, undef, $cmd, undef, $data->{'siteId'});

          $response = (defined($returnVal)) ? $returnVal : getResponse($hash, "DefaultConfirmation");
      }

      # Antwort senden
      respond($hash, $type, $sessionId, $response);
    }

    # Intent von NLU empfangen
    elsif ($topic =~ qr/^hermes\/intent\/.*/) {
        my $intent;
        my $type = ($topic eq "hermes/intent/FHEM:TextCommand") ? "text" : "voice";

        $data->{'requestType'} = $type;
        $intent = $data->{'intent'};

        # Readings updaten
        readingsBeginUpdate($hash);
        readingsBulkUpdate($hash, "lastIntentTopic", $topic);
        readingsBulkUpdate($hash, "lastIntentPayload", encode_json($data));
        readingsEndUpdate($hash, 1);

        # Passenden Intent-Handler aufrufen
        if ($intent eq 'SetOnOff') {
            RHASSPY::handleIntentSetOnOff($hash, $data);
        } elsif ($intent eq 'GetOnOff') {
            RHASSPY::handleIntentGetOnOff($hash, $data);
        } elsif ($intent eq 'SetNumeric') {
            RHASSPY::handleIntentSetNumeric($hash, $data);
        } elsif ($intent eq 'GetNumeric') {
            RHASSPY::handleIntentGetNumeric($hash, $data);
        } elsif ($intent eq 'GetStatus') {
            RHASSPY::handleIntentStatus($hash, $data);
        } elsif ($intent eq 'SetMediaControl') {
            RHASSPY::handleIntentMediaControls($hash, $data);
        } elsif ($intent eq 'SetMediaChannel') {
            RHASSPY::handleIntentMediaChannels($hash, $data);
        } elsif ($intent eq 'GetMediaInfo') {
            RHASSPY::handleIntentMediaInfo($hash, $data);
        } elsif ($intent eq 'SetColor') {
              RHASSPY::handleIntentSetColor($hash, $data);
        } else {
            RHASSPY::handleCustomIntent($hash, $intent, $data);
        }
    }
    # Intent Erkennung ist fehlgeschlagen -> Protokollieren
    elsif ($topic =~ qr/^hermes\/nlu\/intentNotRecognized/) {
        Log3($hash->{NAME}, 3, $hash->{NAME} . " - Not recognized intent: " . $message);
    }
}


# Listening Status setzen
sub setListeningOn {
    my ($hash, $room) = @_;
    my $readingsName = "listening_" . lc(RHASSPY::replaceUmlauts(encode_utf8($room)));

    if (ReadingsVal($hash->{NAME}, $readingsName, 0) != 1) {
        readingsSingleUpdate($hash, $readingsName, 1, 1);
    }
}


# Listening Status setzen
sub setListeningOff {
    my ($timerHash) = @_;
    my $hash = $timerHash->{HASH};
    my $room = $timerHash->{IDENTIFIER};

    my $readingsName = "listening_" . lc(RHASSPY::replaceUmlauts(encode_utf8($room)));

    if (ReadingsVal($hash->{NAME}, $readingsName, 1) != 0) {
        readingsSingleUpdate($hash, $readingsName, 0, 1);
    }
}


# Antwort ausgeben
sub respond($$$$) {
    my ($hash, $type, $sessionId, $response) = @_;
    my $json;

    if ($type eq "voice") {
        my $sendData =  {
            sessionId => $sessionId,
            text => $response
        };

        RHASSPY::mqttPublish($hash, 'hermes/dialogueManager/endSession', $sendData);
        readingsSingleUpdate($hash, "voiceResponse", encode_utf8($response), 1);
    }
    elsif ($type eq "text") {
        Log3($hash->{NAME}, 5, "Response: $response");
        readingsSingleUpdate($hash, "textResponse", encode_utf8($response), 1);
    }
}


# Antworttexte festlegen
sub getResponse($$) {
    my ($hash, $identifier) = @_;
    my $response;

    my %messages = (
        DefaultError => "Da ist etwas schief gegangen.",
        NoActiveMediaDevice => "Kein Wiedergabegerät aktiv.",
        DefaultConfirmation => "Ok."
    );

    my $responsesString = getCmd($hash, $hash->{NAME}, "response", $identifier);

    if (defined($responsesString)) {
        my @responses = split(/\|/, $responsesString);
        my $index = rand(@responses);
        $response = $responses[$index];
    }
    else {
        $response = $messages{$identifier};
    }

    return $response;
}


# Text Kommando an RHASSPY
sub textCommand($$) {
    my ($hash, $text) = @_;

    my $data = { input => $text };
    my $message = encode_json($data);

    # Send fake command, so it's forwarded to NLU
    my $topic = "hermes/intent/FHEM:TextCommand";
    onmessage($hash, $topic, $message);
}


# Sprachausgabe / TTS über RHASSPY
sub say($$) {
    my ($hash, $cmd) = @_;
    my $sendData, my $json;
    my $siteId = "default";
    my $text = $cmd;
    my($unnamedParams, $namedParams) = parseParams($cmd);

    if (defined($namedParams->{'siteId'}) && defined($namedParams->{'text'})) {
        $siteId = $namedParams->{'siteId'};
        $text = $namedParams->{'text'};
    }

    $sendData =  {
        siteId => $siteId,
        text => $text,
        lang => "de",
        id => "0",
        sessionId => "0"
    };

    RHASSPY::mqttPublish($hash, 'hermes/tts/say', $sendData);
}


# Abspielen von Audio Dateien
sub playBytes($$){
    my ($hash, $cmd) = @_;
    my $ifilename = $cmd;
    my $siteId = "default";
    my($unnamedParams, $namedParams) = parseParams($cmd);
    if (defined($namedParams->{'siteId'}) && defined($namedParams->{'file'})) {
        $siteId = $namedParams->{'siteId'};
        $ifilename = $namedParams->{'file'};
    }

    open my $ifile, '<', $ifilename;
    binmode $ifile;

    # Read the binary data
    $_ = do { local $/; <$ifile> };

    RHASSPY::postWavData($hash, $_, $siteId);
}


# Sprachausgabe / TTS über RHASSPY
sub setVolume($$) {
    my ($hash, $params) = @_;
    my $sendData, my $json;
    my $siteId, my $volume;
    my($unnamedParams, $namedParams) = parseParams($params);

    Log3($hash->{NAME}, 5, "Params: $params");

    if (defined($namedParams->{'siteId'}) && defined($namedParams->{'volume'})) {
        $siteId = $namedParams->{'siteId'};
        $volume = $namedParams->{'volume'};

        $sendData =  {
            siteId => $siteId,
            volume => $volume
        };

        RHASSPY::mqttPublish($hash, 'hermes/sound/setvolume', $sendData);
    }
}


# Update der Slots und Sentences von Rhasspy
sub updateModel($) {
    my ($hash) = @_;
    my @articles = allRhasspyArticles();
    my @roomPrepositions = allRhasspyRoomPrepositions();
    my @devicePrepositions = allRhasspyDevicePrepositions();
    my @devices = allRhasspyNames();
    my @rooms = allRhasspyRooms();
    my @channels = allRhasspyChannels();
    my @colors = allRhasspyColors();
    # TODO: Check what 'types' are
    #my @types = allRhasspyTypes();
    my @shortcuts = allRhasspyShortcuts($hash);

    # Build the JSON for the slots
    my $slots;

    if (@articles > 0) { $slots->{'fhem/article'} = \@articles; }
    if (@roomPrepositions > 0) { $slots->{'fhem/roompreposition'} = \@roomPrepositions; }
    if (@devicePrepositions > 0) { $slots->{'fhem/devicepreposition'} = \@devicePrepositions; }
    if (@devices > 0) { $slots->{'fhem/device'} = \@devices; }
    if (@rooms > 0) { $slots->{'fhem/room'} = \@rooms; }
    if (@channels > 0) { $slots->{'fhem/channel'} = \@channels; }
    if (@shortcuts > 0) { $slots->{'fhem/shortcut'} = \@shortcuts; }
    if (@colors > 0) { $slots->{'fhem/color'} = \@colors; }

    # Search for not available slots in sentences and remove these
    my $sentences = allRhasspySentences();
    my @sentenceLines = split(/\n/, $sentences);

    for (my $i = $#sentenceLines; $i > -1; $i--) {
        my $sentence = $sentenceLines[$i];
        my @matches = $sentence =~ /\$([^)\]\s]*)/gm;

        foreach (@matches) {
            my $match = $_;

            if (!(exists $slots->{$match})) {
                splice @sentenceLines, $i, 1;
                last;
            }
        }
    }

    $sentences = join("\n", @sentenceLines);

    # Send slots, sentences and afterwards trigger training
    RHASSPY::postSlots($hash, $slots, sub($) {
        my ($postSlotsError) = @_;

        if ($postSlotsError eq "") {
            RHASSPY::postSentences($hash, $sentences, sub($) {
                my ($postSentencesError) = @_;

                if ($postSentencesError eq "") {
                    RHASSPY::postTrain($hash, $sentences, sub($) {
                        my ($postTrainError) = @_;
                    });
                }
            });
        }
    });
}


# Alle FHEM Slots bei Rhasspy ersetzen
sub postSlots {
    my ($hash, $slots, $completion) = @_;
    my $host = $hash->{HOST};
    my $port = $hash->{PORT};
    my $url = "http://$host:$port/api/slots?overwrite_all=true";
    my $json = encode_json($slots);

    Log3($hash->{NAME}, 5, "postSlots - $json");
    Log3($hash->{NAME}, 5, "postSlots - hex: " . unpack("H*", $json));

    my $params = {
        url        => $url,
        method     => "POST",
        timeout    => 10,
        noshutdown => 1,
        data       => $json,
        hash       => $hash,
        header     => "Content-Type: application/json"
    };

    $params->{callback} = sub($$$) {
        my ($param, $err, $data) = @_;

        if ($err ne "") {
            Log3($hash->{NAME}, 1, "Post slots error: $err");
        }

        $completion->($err);
    };

    HttpUtils_NonblockingGet($params);
}


# Alle FHEM Sentences bei Rhasspy ersetzen
sub postSentences {
    my ($hash, $sentences, $completion) = @_;
    my $host = $hash->{HOST};
    my $port = $hash->{PORT};
    my $url = "http://$host:$port/api/sentences";

    Log3($hash->{NAME}, 5, "postSentences - sentences hex: " . unpack("H*", $sentences));

    my $data = {
        'intents/fhem_sentences.ini' => $sentences
    };
    my $json = encode_json($data);

    Log3($hash->{NAME}, 5, "postSentences - json: $json");
    Log3($hash->{NAME}, 5, "postSentences - json hex: " . unpack("H*", $json));

    my $params = {
        url        => $url,
        method     => "POST",
        timeout    => 10,
        noshutdown => 1,
        data       => $json,
        hash       => $hash,
        header     => "Content-Type: application/json"
    };

    Log3($hash->{NAME}, 5, "Posting train");

    $params->{callback} = sub($$$) {
        my ($param, $err, $data) = @_;

        if ($err ne "") {
            Log3($hash->{NAME}, 1, "Post senteces error: $err");
        }

        $completion->($err);
    };

    HttpUtils_NonblockingGet($params);
}


# Rhasspy Training starten
sub postTrain {
    my ($hash, $sentences, $completion) = @_;
    my $host = $hash->{HOST};
    my $port = $hash->{PORT};
    my $url = "http://$host:$port/api/train";

    my $params = {
        url        => $url,
        method     => "POST",
        timeout    => 10000,
        noshutdown => 1,
        data       => undef,
        hash       => $hash,
        header     => "Content-Type: application/json"
    };

    $params->{callback} = sub($$$) {
        my ($param, $err, $data) = @_;

        if ($err ne "") {
            Log3($hash->{NAME}, 1, "Post train error: $err");
        }

        $completion->($err);
    };

    HttpUtils_NonblockingGet($params);
}


# WAV Daten an Rhasspy senden
sub postWavData {
    my ($hash,  $data, $siteId) = @_;
    my $host = $hash->{HOST};
    my $port = $hash->{PORT};
    my $url = "http://$host:$port/api/play-wav?siteId=" . urlEncode(encode_utf8($siteId));

    my $params = {
        url        => $url,
        method     => "POST",
        timeout    => 10,
        noshutdown => 1,
        data       => $data,
        hash       => $hash,
        header     => "Content-Type: audio/wav"
    };

    $params->{callback} = sub($$$) {
        my ($param, $err, $data) = @_;

        if ($err ne "") {
            Log3($hash->{NAME}, 1, "Post wav data error: $err");
        }
    };

    HttpUtils_NonblockingGet($params);
}


# Eingehender Custom-Intent
sub handleCustomIntent($$$) {
    my ($hash, $intentName, $data) = @_;
    my @intents, my $intent;
    my $intentsString = decode_utf8(AttrVal($hash->{NAME},"rhasspyIntents",undef));
    my $response;
    my $error;

    Log3($hash->{NAME}, 5, "handleCustomIntent called");

    # Suchen ob ein passender Custom Intent existiert
    @intents = split(/\n/, $intentsString);
    foreach (@intents) {
        next unless $_ =~ qr/^$intentName/;

        $intent = $_;
        Log3($hash->{NAME}, 5, "rhasspyIntent selected: $_");
    }

    # Gerät setzen falls Slot Device vorhanden
    if (exists($data->{'Device'})) {
      my $room = roomName($hash, $data);
      my $device = getDeviceByName($hash, $room, $data->{'Device'});
      $data->{'Device'} = $device;
    }

    # Custom Intent Definition Parsen
    if ($intent =~ qr/^$intentName=.*\(.*\)/) {
        my @tokens = split(/=|\(|\)/, $intent);
        my $subName =  "main::" . $tokens[1] if (@tokens > 0 && defined($tokens[1]));
        my @paramNames = split(/,/, $tokens[2]) if (@tokens > 1 && defined($tokens[2]));

        if (defined($subName)) {
            my @params = map { $data->{$_} } @paramNames;

            # Sub aus dem Custom Intent aufrufen
            eval {
                Log3($hash->{NAME}, 5, "Calling sub: $subName");

                no strict 'refs';
                $response = $subName->(@params);
            };

            if ($@) {
                Log3($hash->{NAME}, 5, $@);
            }
        }
        $response = getResponse($hash, "DefaultError") if (!defined($response));

        # Antwort senden
        respond ($hash, $data->{'requestType'}, $data->{sessionId}, $response);
    }
}


# Eingehende "SetOnOff" Intents bearbeiten
sub handleIntentSetOnOff($$) {
    my ($hash, $data) = @_;
    my $value, my $numericValue, my $device, my $room;
    my $mapping;
    my $response = getResponse($hash, "DefaultError");

    Log3($hash->{NAME}, 5, "handleIntentSetOnOff called");

    # Mindestens Gerät und Wert müssen übergeben worden sein
    if (exists($data->{'Device'}) && exists($data->{'Value'})) {
        $room = roomName($hash, $data);
        $value = $data->{'Value'};
        $device = getDeviceByName($hash, $room, $data->{'Device'});
        $mapping = getMapping($hash, $device, "SetOnOff", undef);

        # Mapping gefunden?
        if (defined($device) && defined($mapping)) {
            my $cmdOn  = (defined($mapping->{'cmdOn'}))  ? $mapping->{'cmdOn'}  :  "on";
            my $cmdOff = (defined($mapping->{'cmdOff'})) ? $mapping->{'cmdOff'} : "off";
            my $cmd = ($value eq 'an') ? $cmdOn : $cmdOff;

            # Cmd ausführen
            runCmd($hash, $device, $cmd);

            # Antwort bestimmen
            $numericValue = ($value eq 'an') ? 1 : 0;
            if (defined($mapping->{'response'})) { $response = getValue($hash, $device, $mapping->{'response'}, $numericValue, $room); }
            else { $response = getResponse($hash, "DefaultConfirmation"); }
        }
    }
    # Antwort senden
    respond ($hash, $data->{'requestType'}, $data->{sessionId}, $response);
}


# Eingehende "GetOnOff" Intents bearbeiten
sub handleIntentGetOnOff($$) {
    my ($hash, $data) = @_;
    my $value, my $device, my $room, my $status;
    my $mapping;
    my $response = getResponse($hash, "DefaultError");

    Log3($hash->{NAME}, 5, "handleIntentGetOnOff called");

    # Mindestens Gerät und Status-Art wurden übergeben
    if (exists($data->{'Device'}) && exists($data->{'Status'})) {
        $room = roomName($hash, $data);
        $device = getDeviceByName($hash, $room, $data->{'Device'});
        $mapping = getMapping($hash, $device, "GetOnOff", undef);
        $status = $data->{'Status'};

        # Mapping gefunden?
        if (defined($mapping)) {
            # Gerät ein- oder ausgeschaltet?
            $value = getOnOffState($hash, $device, $mapping);

            # Antwort bestimmen
            if    (defined($mapping->{'response'})) { $response = getValue($hash, $device, $mapping->{'response'}, $value, $room); }
            elsif ($status =~ m/^(an|aus)$/ && $value == 1) { $response = $data->{'Device'} . " ist eingeschaltet"; }
            elsif ($status =~ m/^(an|aus)$/ && $value == 0) { $response = $data->{'Device'} . " ist ausgeschaltet"; }
            elsif ($status =~ m/^(auf|zu)$/ && $value == 1) { $response = $data->{'Device'} . " ist geöffnet"; }
            elsif ($status =~ m/^(auf|zu)$/ && $value == 0) { $response = $data->{'Device'} . " ist geschlossen"; }
            elsif ($status =~ m/^(eingefahren|ausgefahren)$/ && $value == 1) { $response = $data->{'Device'} . " ist eingefahren"; }
            elsif ($status =~ m/^(eingefahren|ausgefahren)$/ && $value == 0) { $response = $data->{'Device'} . " ist ausgefahren"; }
            elsif ($status =~ m/^(läuft|fertig)$/ && $value == 1) { $response = $data->{'Device'} . " läuft noch"; }
            elsif ($status =~ m/^(läuft|fertig)$/ && $value == 0) { $response = $data->{'Device'} . " ist fertig"; }
        }
    }
    # Antwort senden
    respond ($hash, $data->{'requestType'}, $data->{sessionId}, $response);
}


# Eingehende "SetNumeric" Intents bearbeiten
sub handleIntentSetNumeric($$) {
    my ($hash, $data) = @_;
    my $value, my $device, my $room, my $change, my $type, my $unit;
    my $mapping;
    my $validData = 0;
    my $response = getResponse($hash, "DefaultError");

    Log3($hash->{NAME}, 5, "handleIntentSetNumeric called");

    # Mindestens Device und Value angegeben -> Valid (z.B. Deckenlampe auf 20%)
    $validData = 1 if (exists($data->{'Device'}) && exists($data->{'Value'}));
    # Mindestens Device und Change angegeben -> Valid (z.B. Radio lauter)
    $validData = 1 if (exists($data->{'Device'}) && exists($data->{'Change'}));
    # Nur Change für Lautstärke angegeben -> Valid (z.B. lauter)
    $validData = 1 if (!exists($data->{'Device'}) && defined($data->{'Change'}) && $data->{'Change'} =~ m/^(lauter|leiser)$/i);
    # Nur Type = Lautstärke und Value angegeben -> Valid (z.B. Lautstärke auf 10)
    $validData = 1 if (!exists($data->{'Device'}) && defined($data->{'Type'}) && $data->{'Type'} =~ m/^Lautstärke$/i && exists($data->{'Value'}));

    if ($validData == 1) {
        $unit = $data->{'Unit'};
        $type = $data->{'Type'};
        $value = $data->{'Value'};
        $change = $data->{'Change'};
        $room = roomName($hash, $data);

        # Type nicht belegt -> versuchen Type über change Value zu bestimmen
        if (!defined($type) && defined($change)) {
            if    ($change =~ m/^(kälter|wärmer)$/)  { $type = "Temperatur"; }
            elsif ($change =~ m/^(dunkler|heller)$/) { $type = "Helligkeit"; }
            elsif ($change =~ m/^(lauter|leiser)$/)  { $type = "Lautstärke"; }
        }

        # Gerät über Name suchen, oder falls über Lautstärke ohne Device getriggert wurde das ActiveMediaDevice suchen
        if (exists($data->{'Device'})) {
            $device = getDeviceByName($hash, $room, $data->{'Device'});
        } elsif (defined($type) && $type =~ m/^Lautstärke$/i) {
            $device = getActiveDeviceForIntentAndType($hash, $room, "SetNumeric", $type);
            $response = getResponse($hash, "NoActiveMediaDevice") if (!defined($device));
        }

        if (defined($device)) {
            $mapping = getMapping($hash, $device, "SetNumeric", $type);

            # Mapping und Gerät gefunden -> Befehl ausführen
            if (defined($mapping) && defined($mapping->{'cmd'})) {
                my $cmd     = $mapping->{'cmd'};
                my $part    = $mapping->{'part'};
                my $minVal  = (defined($mapping->{'minVal'})) ? $mapping->{'minVal'} : 0; # Rhasspy kann keine negativen Nummern bisher, daher erzwungener minVal
                my $maxVal  = $mapping->{'maxVal'};
                my $diff    = (defined($value)) ? $value : ((defined($mapping->{'step'})) ? $mapping->{'step'} : 10);
                my $up      = (defined($change) && ($change =~ m/^(höher|heller|lauter|wärmer)$/)) ? 1 : 0;
                my $forcePercent = (defined($mapping->{'map'}) && lc($mapping->{'map'}) eq "percent") ? 1 : 0;

                # Alten Wert bestimmen
                my $oldVal  = getValue($hash, $device, $mapping->{'currentVal'});
                if (defined($part)) {
                    my @tokens = split(/ /, $oldVal);
                    $oldVal = $tokens[$part] if (@tokens >= $part);
                }

                # Neuen Wert bestimmen
                my $newVal;
                # Direkter Stellwert ("Stelle Lampe auf 50")
                if ($unit ne "Prozent" && defined($value) && !defined($change) && !$forcePercent) {
                    $newVal = $value;
                }
                # Direkter Stellwert als Prozent ("Stelle Lampe auf 50 Prozent", oder "Stelle Lampe auf 50" bei forcePercent)
                elsif (defined($value) && ((defined($unit) && $unit eq "Prozent") || $forcePercent) && !defined($change) && defined($minVal) && defined($maxVal)) {
                    # Wert von Prozent in Raw-Wert umrechnen
                    $newVal = $value;
                    $newVal =   0 if ($newVal <   0);
                    $newVal = 100 if ($newVal > 100);
                    $newVal = main::round((($newVal * (($maxVal - $minVal) / 100)) + $minVal), 0);
                }
                # Stellwert um Wert x ändern ("Mache Lampe um 20 heller" oder "Mache Lampe heller")
                elsif ((!defined($unit) || $unit ne "Prozent") && defined($change) && !$forcePercent) {
                    $newVal = ($up) ? $oldVal + $diff : $oldVal - $diff;
                }
                # Stellwert um Prozent x ändern ("Mache Lampe um 20 Prozent heller" oder "Mache Lampe um 20 heller" bei forcePercent oder "Mache Lampe heller" bei forcePercent)
                elsif (($unit eq "Prozent" || $forcePercent) && defined($change)  && defined($minVal) && defined($maxVal)) {
                    my $diffRaw = main::round((($diff * (($maxVal - $minVal) / 100)) + $minVal), 0);
                    $newVal = ($up) ? $oldVal + $diffRaw : $oldVal - $diffRaw;
                }

                if (defined($newVal)) {
                    # Begrenzung auf evtl. gesetzte min/max Werte
                    $newVal = $minVal if (defined($minVal) && $newVal < $minVal);
                    $newVal = $maxVal if (defined($maxVal) && $newVal > $maxVal);

                    # Cmd ausführen
                    runCmd($hash, $device, $cmd, $newVal);

                    # Antwort festlegen
                    if (defined($mapping->{'response'})) { $response = getValue($hash, $device, $mapping->{'response'}, $newVal, $room); }
                    else { $response = getResponse($hash, "DefaultConfirmation"); }
                }
            }
        }
    }
    # Antwort senden
    respond ($hash, $data->{'requestType'}, $data->{sessionId}, $response);
}


# Eingehende "GetNumeric" Intents bearbeiten
sub handleIntentGetNumeric($$) {
    my ($hash, $data) = @_;
    my $value, my $device, my $room, my $type;
    my $mapping;
    my $response = getResponse($hash, "DefaultError");

    Log3($hash->{NAME}, 5, "handleIntentGetNumeric called");

    # Mindestens Type oder Device muss existieren
    if (exists($data->{'Type'}) || exists($data->{'Device'})) {
        $type = $data->{'Type'};
        $room = roomName($hash, $data);

        # Passendes Gerät suchen
        if (exists($data->{'Device'})) {
            $device = getDeviceByName($hash, $room, $data->{'Device'});
        } else {
            $device = getDeviceByIntentAndType($hash, $room, "GetNumeric", $type);
        }

        $mapping = getMapping($hash, $device, "GetNumeric", $type) if (defined($device));

        # Mapping gefunden
        if (defined($mapping)) {
            my $part = $mapping->{'part'};
            my $minVal  = $mapping->{'minVal'};
            my $maxVal  = $mapping->{'maxVal'};
            my $mappingType = $mapping->{'type'};
            my $forcePercent = (defined($mapping->{'map'}) && lc($mapping->{'map'}) eq "percent" && defined($minVal) && defined($maxVal)) ? 1 : 0;
            my $isNumber;

            # Zurückzuliefernden Wert bestimmen
            $value = getValue($hash, $device, $mapping->{'currentVal'});
            if (defined($part)) {
              my @tokens = split(/ /, $value);
              $value = $tokens[$part] if (@tokens >= $part);
            }
            $value = main::round((($value * (($maxVal - $minVal) / 100)) + $minVal), 0) if ($forcePercent);
            $isNumber = main::looks_like_number($value);

            # Punkt durch Komma ersetzen in Dezimalzahlen
            $value =~ s/\./\,/g;

            # Antwort falls Custom Response definiert ist
            if    (defined($mapping->{'response'})) { $response = getValue($hash, $device, $mapping->{'response'}, $value, $room); }

            # Antwort falls mappingType matched
            elsif ($mappingType =~ m/^(Helligkeit|Lautstärke|Sollwert)$/i) { $response = $data->{'Device'} . " ist auf $value gestellt."; }
            elsif ($mappingType =~ m/^Temperatur$/i) { $response = "Die Temperatur von " . (exists $data->{'Device'} ? $data->{'Device'} : $room) . " beträgt $value" . ($isNumber ? " Grad" : ""); }
            elsif ($mappingType =~ m/^Luftfeuchtigkeit$/i) { $response = "Die Luftfeuchtigkeit von " . (exists $data->{'Device'} ? $data->{'Device'} : $room) . " beträgt $value" . ($isNumber ? " Prozent" : ""); }
            elsif ($mappingType =~ m/^Batterie$/i) { $response = "Der Batteriestand von " . (exists $data->{'Device'} ? $data->{'Device'} : $room) . ($isNumber ?  " beträgt $value Prozent" : " ist $value"); }
            elsif ($mappingType =~ m/^Wasserstand$/i) { $response = "Der Wasserstand von " . (exists $data->{'Device'} ? $data->{'Device'} : $room) . " beträgt $value"; }

            # Andernfalls Antwort falls type aus Intent matched
            elsif ($type =~ m/^(Helligkeit|Lautstärke|Sollwert)$/) { $response = $data->{'Device'} . " ist auf $value gestellt."; }
            elsif ($type =~ m/^Temperatur$/i) { $response = "Die Temperatur von " . (exists $data->{'Device'} ? $data->{'Device'} : $room) . " beträgt $value" . ($isNumber ? " Grad" : ""); }
            elsif ($type =~ m/^Luftfeuchtigkeit$/i) { $response = "Die Luftfeuchtigkeit von " . (exists $data->{'Device'} ? $data->{'Device'} : $room) . " beträgt $value" . ($isNumber ? " Prozent" : ""); }
            elsif ($type =~ m/^Batterie$/i) { $response = "Der Batteriestand von " . (exists $data->{'Device'} ? $data->{'Device'} : $room) . ($isNumber ?  " beträgt $value Prozent" : " ist $value"); }
            elsif ($type =~ m/^Wasserstand$/i) { $response = "Der Wasserstand von " . (exists $data->{'Device'} ? $data->{'Device'} : $room) . " beträgt $value"; }

            # Antwort wenn Custom Type
            elsif (defined($mappingType)) { $response = "$mappingType von " . (exists $data->{'Device'} ? $data->{'Device'} : $room) . " beträgt $value"; }

            # Standardantwort falls der Type überhaupt nicht bestimmt werden kann
            else { $response = "Der Wert von " . $data->{'Device'} . " beträgt $value."; }
        }
    }
    # Antwort senden
    respond ($hash, $data->{'requestType'}, $data->{sessionId}, $response);
}


# Eingehende "Status" Intents bearbeiten
sub handleIntentStatus($$) {
    my ($hash, $data) = @_;
    my $value, my $device, my $room;
    my $mapping;
    my $response = getResponse($hash, "DefaultError");

    Log3($hash->{NAME}, 5, "handleIntentStatus called");

    # Mindestens Device muss existieren
    if (exists($data->{'Device'})) {
        $room = roomName($hash, $data);
        $device = getDeviceByName($hash, $room, $data->{'Device'});
        $mapping = getMapping($hash, $device, "Status", undef);

        if (defined($mapping->{'response'})) {
            $response = getValue($hash, $device, $mapping->{'response'},undef,  $room);
        }
    }
    # Antwort senden
    respond ($hash, $data->{'requestType'}, $data->{sessionId}, $response);
}


# Eingehende "MediaControls" Intents bearbeiten
sub handleIntentMediaControls($$) {
    my ($hash, $data) = @_;
    my $command, my $device, my $room;
    my $mapping;
    my $response = getResponse($hash, "DefaultError");

    Log3($hash->{NAME}, 5, "handleIntentMediaControls called");

    # Mindestens Kommando muss übergeben worden sein
    if (exists($data->{'Command'})) {
        $room = roomName($hash, $data);
        $command = $data->{'Command'};

        # Passendes Gerät suchen
        if (exists($data->{'Device'})) {
            $device = getDeviceByName($hash, $room, $data->{'Device'});
        } else {
            $device = getActiveDeviceForIntentAndType($hash, $room, "MediaControls", undef);
            $response = getResponse($hash, "NoActiveMediaDevice") if (!defined($device));
        }

        $mapping = getMapping($hash, $device, "MediaControls", undef);

        if (defined($device) && defined($mapping)) {
            my $cmd;

            if    ($command =~ m/^play$/i)   { $cmd = $mapping->{'cmdPlay'}; }
            elsif ($command =~ m/^pause$/i)  { $cmd = $mapping->{'cmdPause'}; }
            elsif ($command =~ m/^stop$/i)   { $cmd = $mapping->{'cmdStop'}; }
            elsif ($command =~ m/^vor$/i)    { $cmd = $mapping->{'cmdFwd'}; }
            elsif ($command =~ m/^zurück$/i) { $cmd = $mapping->{'cmdBack'}; }

            if (defined($cmd)) {
                # Cmd ausführen
                runCmd($hash, $device, $cmd);

                # Antwort festlegen
                if (defined($mapping->{'response'})) { $response = getValue($hash, $device, $mapping->{'response'}, $command, $room); }
                else { $response = getResponse($hash, "DefaultConfirmation"); }
            }
        }
    }
    # Antwort senden
    respond ($hash, $data->{'requestType'}, $data->{sessionId}, $response);
}


# Eingehende "MediaChannels" Intents bearbeiten
sub handleIntentMediaChannels($$) {
    my ($hash, $data) = @_;
    my $channel, my $device, my $room;
    my $cmd;
    my $response = getResponse($hash, "DefaultError");

    Log3($hash->{NAME}, 5, "handleIntentMediaChannels called");

    # Mindestens Channel muss übergeben worden sein
    if (exists($data->{'Channel'})) {
        $room = roomName($hash, $data);
        $channel = $data->{'Channel'};

        # Passendes Gerät suchen
        if (exists($data->{'Device'})) {
            $device = getDeviceByName($hash, $room, $data->{'Device'});
        } else {
            $device = getDeviceByMediaChannel($hash, $room, $channel);
        }

        $cmd = getCmd($hash, $device, "rhasspyChannels", $channel, undef);

        if (defined($device) && defined($cmd)) {
            $response = getResponse($hash, "DefaultConfirmation");

            # Cmd ausführen
            runCmd($hash, $device, $cmd);
        }
    }
    # Antwort senden
    respond ($hash, $data->{'requestType'}, $data->{sessionId}, $response);
}


# Eingehende "GetMediaInfo" Intents bearbeiten
sub handleIntentMediaInfo($$) {
    my ($hash, $data) = @_;
    my $questionType, my $device, my $room;
    my $mapping;
    my $response = getResponse($hash, "DefaultError");

    Log3($hash->{NAME}, 5, "handleIntentMediaInfo called");

    # Mindestens die Art der Frage muss übergeben worden sein
    if (exists($data->{'QuestionType'})) {
        $room = roomName($hash, $data);
        $questionType = $data->{'QuestionType'};

        # Passendes Gerät suchen
        if (exists($data->{'Device'})) {
            $device = getDeviceByName($hash, $room, $data->{'Device'});
        } else {
            $device = getActiveDeviceForIntentAndType($hash, $room, "GetMediaInfo", undef);
        }

        $response = getResponse($hash, "NoActiveMediaDevice");

        if (defined($device)) {
            $response = "Das weiß ich leider nicht.";
            $mapping = getMapping($hash, $device, "GetMediaInfo", undef);

            if (defined($mapping)) {
                my $mappingKey;
                if ($questionType eq "zusammenfassung") {
                    $mappingKey = 'summary';
                }
                elsif ($questionType eq "dauer") {
                    $mappingKey = 'duration';
                }

                if (defined($mappingKey) && exists($mapping->{$mappingKey})) {
                    my $value = getValue($hash, $device, $mapping->{$mappingKey});

                    if (defined($value)) {
                        $response = $value;
                    }
                }
            }
        }
    }

    # Antwort senden
    respond ($hash, $data->{'requestType'}, $data->{sessionId}, $response);
}


# Eingehende "SetColor" Intents bearbeiten
sub handleIntentSetColor($$) {
    my ($hash, $data) = @_;
    my $color, my $device, my $room;
    my $cmd;
    my $response = getResponse($hash, "DefaultError");

    Log3($hash->{NAME}, 5, "handleIntentSetColor called");

    # Mindestens Device und Color muss übergeben worden sein
    if (exists($data->{'Color'}) && exists($data->{'Device'})) {
        $room = roomName($hash, $data);
        $color = $data->{'Color'};

        # Passendes Gerät & Cmd suchen
        $device = getDeviceByName($hash, $room, $data->{'Device'});
        $cmd = getCmd($hash, $device, "rhasspyColors", $color, undef);

        if (defined($device) && defined($cmd)) {
            $response = getResponse($hash, "DefaultConfirmation");

            # Cmd ausführen
            runCmd($hash, $device, $cmd);
        }
    }
    # Antwort senden
    respond ($hash, $data->{'requestType'}, $data->{sessionId}, $response);
}


# Abgespeckte Kopie von ReplaceSetMagic aus fhem.pl
sub	ReplaceReadingsVal($@) {
    my $hash = shift;
    my $a = join(" ", @_);

    sub readingsVal($$$$$) {
        my ($all, $t, $d, $n, $s, $val) = @_;
        my $hash = $defs{$d};
        return $all if(!$hash);

        if(!$t || $t eq "r:") {
            my $r = $hash->{READINGS};
            if($s && ($s eq ":t" || $s eq ":sec")) {
                return $all if (!$r || !$r->{$n});
                $val = $r->{$n}{TIME};
                $val = int(gettimeofday()) - time_str2num($val) if($s eq ":sec");
                return $val;
            }
            $val = $r->{$n}{VAL} if($r && $r->{$n});
        }
        $val = $hash->{$n}   if(!defined($val) && (!$t || $t eq "i:"));
        $val = $main::attr{$d}{$n} if(!defined($val) && (!$t || $t eq "a:") && $main::attr{$d});
        return $all if(!defined($val));

        if($s && $s =~ /:d|:r|:i/ && $val =~ /(-?\d+(\.\d+)?)/) {
            $val = $1;
            $val = int($val) if ( $s eq ":i" );
            $val = round($val, defined($1) ? $1 : 1) if($s =~ /^:r(\d)?/);
        }
        return $val;
    }

    $a =~s/(\[([ari]:)?([a-zA-Z\d._]+):([a-zA-Z\d._\/-]+)(:(t|sec|i|d|r|r\d))?\])/readingsVal($1,$2,$3,$4,$5)/eg;
    return $a;
}

sub unicodeDecode {
    my ($string) = @_;
    $string =~ s/\\u(....)/chr(hex($1))/eg;

    return $string;
}


#######################################
#       Timer Functions
#######################################

sub identifiableInternalTimer {
   my ($identifier, $tim, $callback, $hash, $waitIfInitNotDone) = @_;

   my $mHash;
   if ($identifier eq "") {
      $mHash = $hash;
   } else {
      my $timerName = "$hash->{NAME}_$identifier";
      if (exists  ($hash->{TIMER}{$timerName})) {
          $mHash = $hash->{TIMER}{$timerName};
      } else {
          $mHash = { HASH=>$hash, NAME=>$timerName, IDENTIFIER=>$identifier};
          $hash->{TIMER}{$timerName} = $mHash;
      }
   }
   InternalTimer($tim, $callback, $mHash, $waitIfInitNotDone);
}
################################################################################
sub removeIdentifiableInternalTimer {
   my ($identifier, $hash, $function) = @_;

   my $timerName = "$hash->{NAME}_$identifier";
   if ($identifier eq "") {
      RemoveInternalTimer($hash, $function);
   } else {
      my $myHash = $hash->{TIMER}{$timerName};
      if (defined($myHash)) {
         delete $hash->{TIMER}{$timerName};
         RemoveInternalTimer($myHash, $function);
      }
   }
}
#######################################
#       Websocket Functions
#######################################

sub ioDevReconnect {
    my ($hash) = @_;

    # IoDev schließen und anschließend öffnen
    DevIo_CloseDev($hash);

    if (AttrVal($hash->{NAME}, "disable", 0) == 0) {
        return DevIo_OpenDev($hash, 1, "RHASSPY::ioDevOpened");
    }
    else {
        return undef
    }
}

sub ioDevReady {
    my ($hash) = @_;

    if (AttrVal($hash->{NAME}, "disable", 0) == 0 && $hash->{STATE} eq "disconnected") {
        return DevIo_OpenDev($hash, 1, "RHASSPY::ioDevOpened");
    }
    else {
        return undef
    }
}

sub ioDevOpened {
    my ($hash) = @_;

    # Connection established, try to do a handshake
    RHASSPY::websocketWriteHandshake($hash, "/api/mqtt");
}

sub ioDevRead {
    my $hash = shift;
    my $name = $hash->{NAME};
    my $buf;

    Log3($name, 5, "ioDevRead - ReadFn started");
    $buf = DevIo_SimpleRead($hash);

    return Log3($name, 3, "ioDevRead - no data received") unless( defined $buf);

    if ($hash->{HELPER}{WEBSOCKETS}) {
      Log3($name, 4, "ioDevRead - received data, start response processing:\n".unpack("H*", $buf));
      RHASSPY::websocketDecode($hash, $buf);
    } elsif($buf =~ /HTTP\/1.1 101/) {
      Log3($name, 4, "ioDevRead - received HTTP data string, start response processing:\n$buf");
      RHASSPY::websocketCheckHandshake($hash, $buf);
    } else {
      Log3($name, 1, "ioDevRead - corrupted data found:\n$buf");
    }
}

sub ioDevWrite {
    my ($hash, $string) = @_;
    my $name = $hash->{NAME};

    Log3($name, 4, "ioDevWrite - WriteFn called:\n$string");
    DevIo_SimpleWrite($hash, $string, 0);

    return undef;
}

sub websocketWriteHandshake {
    my ($hash, $path) = @_;
    my $name = $hash->{NAME};
    my $host = $hash->{HOST};
    my $port = $hash->{PORT};
    my $wsKey = encode_base64(gettimeofday(), '');
    my $now = time();
    my $date = FmtDateTimeRFC1123($now);

    my $wsHandshakeCmd = "GET $path HTTP/1.1\r\n";
    $wsHandshakeCmd .= "Host: $host:$port\r\n";
    $wsHandshakeCmd .= "Sec-WebSocket-Key: $wsKey\r\n";
    $wsHandshakeCmd .= "Sec-WebSocket-Version: 13\r\n";
    $wsHandshakeCmd .= "Upgrade: websocket\r\n";
    $wsHandshakeCmd .= "Origin: ws://$host:$port$path\r\n";
    $wsHandshakeCmd .= "Date: $date\r\n";
    $wsHandshakeCmd .= "Connection: Upgrade\r\n";
    $wsHandshakeCmd .= "\r\n";

    readingsSingleUpdate($hash, 'state', 'ws_connected', 0);
    $hash->{HELPER}{WEBSOCKETS} = '0';

    Log3($name, 4, "websocketWriteHandshake - Starting Websocket Handshake");
    RHASSPY::ioDevWrite($hash, $wsHandshakeCmd);

    $hash->{HELPER}{wsKey}  = $wsKey;

    return undef;
}

sub websocketCheckHandshake {
    my ($hash, $response) = @_;
    my $name = $hash->{NAME};

    # header in Hash wandeln
    my %header = ();
    foreach my $line (split("\r\n", $response)) {
        my ($key, $value) = split( ": ", $line);
        next if(!$value);
        $value =~ s/^ //;
        Log3($name, 4, "websocketCheckHandshake - headertohash |$key|$value|");
        $header{lc($key)} = $value;
    }

    # check handshake
    if (defined($header{'sec-websocket-accept'})) {
        my $keyAccept   = $header{'sec-websocket-accept'};
        Log3($name, 5, "websocketCheckHandshake - keyAccept: $keyAccept");
        my $wsKey = $hash->{HELPER}{wsKey};
        my $expectedResponse = trim(encode_base64(pack('H*', sha1_hex(trim($wsKey)."258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))));
        if ($keyAccept eq $expectedResponse) {
            Log3($name, 4, "websocketCheckHandshake - Successful WS connection to $hash->{DeviceName}");
            readingsSingleUpdate($hash, 'state', 'ws_connected', 1);
            $hash->{HELPER}{WEBSOCKETS} = '1';

            # Handshake successful, so subscribe to all topics
            RHASSPY::mqttSubscribeTopics($hash);
        } else {
            DevIo_CloseDev($hash);
            Log3($name, 3, "websocketCheckHandshake - ERROR: Unsucessfull WS connection to $hash->{DeviceName}");
            readingsSingleUpdate($hash, 'state', 'ws_handshake-error', 1);
        }
    }

    return undef;
}

sub websocketDecode {
    my ($hash, $wsString) = @_;
    my $name = $hash->{NAME};

    Log3($name, 5, "websocketDecode - String:\n" . $wsString);

    while (length $wsString) {
        my $FIN =    (ord(substr($wsString,0,1)) & 0b10000000) >> 7;
        my $OPCODE = (ord(substr($wsString,0,1)) & 0b00001111);
        my $masked = (ord(substr($wsString,1,1)) & 0b10000000) >> 7;
        my $len =    (ord(substr($wsString,1,1)) & 0b01111111);
        Log3($name, 4, "websocketDecode - FIN:$FIN OPCODE:$OPCODE MASKED:$masked LEN:$len");

        my $offset = 2;
        if ($len == 126) {
            $len = unpack 'n', substr($wsString,$offset,2);
            $offset += 2;
        } elsif ($len == 127) {
            $len = unpack 'q', substr($wsString,$offset,8);
            $offset += 8;
        }
        my $mask;
        if ($masked) {                     # Mask auslesen falls Masked Bit gesetzt
            $mask = substr($wsString, $offset, 4);
            $offset += 4;
        }
        #String kürzer als Längenangabe -> Zwischenspeichern?
        if (length($wsString) < $offset + $len) {
            Log3($name, 3, "websocketDecode - Incomplete:\n" . $wsString);
            return;
        }
        my $payload = substr($wsString, $offset, $len);     # Daten aus String extrahieren
        if ($masked) {                      # Daten demaskieren falls maskiert
           $payload = RHASSPY::websocketMasking($payload, $mask);
        }
        Log3($name, 5, "websocketDecode - Payload:\n" . $payload);
        $wsString = substr($wsString, $offset+$len);       # ausgewerteten Stringteil entfernen
        if ($FIN) {
            RHASSPY::websocketPong($hash) if ($OPCODE == $websocketOpcode{"ping"});
        }

        RHASSPY::mqttDecode($hash, $payload);
    }
}

# 0                   1                   2                   3
# 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
# +-+-+-+-+-------+-+-------------+-------------------------------+
# |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
# |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
# |N|V|V|V|       |S|             |   (if payload len==126/127)   |
# | |1|2|3|       |K|             |                               |
# +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
# |     Extended payload length continued, if payload len == 127  |
# + - - - - - - - - - - - - - - - +-------------------------------+
# |                               |Masking-key, if MASK set to 1  |
# +-------------------------------+-------------------------------+
##  | Masking-key (continued)       |          Payload Data         |
# +-------------------------------- - - - - - - - - - - - - - - - +
# :                     Payload Data continued ...                :
# + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
# |                     Payload Data continued ...                |
# +---------------------------------------------------------------+
# https://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-17
sub websocketEncode {
    my ($hash, $payload, $type, $masked) = @_;
    my $name = $hash->{NAME};
    $type //= "text";
    $masked //= 1;    # Mask   If set to 1, a masking key is present in masking-key. 1 for all frames sent from client to server
    my $RSV = 0;
    my $FIN = 1;    # FIN    Indicates that this is the final fragment in a message. The first fragment MAY also be the final fragment.
    my $MAX_PAYLOAD_SIZE = 65536;
    my $wsString ='';
    $wsString .= pack 'C', ($websocketOpcode{$type} | $RSV | ($FIN ? 128 : 0));
    my $len = length($payload);

    Log3($name, 4, "websocketEncode - Payload: " . $payload);
    #return "payload too big" if ($len > $MAX_PAYLOAD_SIZE);

    if ($len <= 125) {
        $len |= 0x80 if $masked;
        $wsString .= pack 'C', $len;
    } elsif ($len <= 0xffff) {
        $wsString .= pack 'C', 126 + ($masked ? 128 : 0);
        $wsString .= pack 'n', $len;
    } else {
        $wsString .= pack 'C', 127 + ($masked ? 128 : 0);
        $wsString .= pack 'N', $len >> 32;
        $wsString .= pack 'N', ($len & 0xffffffff);
    }
    if ($masked) {
        my $mask = pack 'N', int(rand(2**32));
        $wsString .= $mask;
        $wsString .= RHASSPY::websocketMasking($payload, $mask);
    } else {
        $wsString .= $payload;
    }

    Log3($name, 5, "websocketEncode - String: " . unpack('H*', $wsString));
    RHASSPY::ioDevWrite($hash, $wsString);
}

sub websocketMasking {
    my ($payload, $mask) = @_;
    $mask = $mask x (int(length($payload) / 4) + 1);
    $mask = substr($mask, 0, length($payload));
    $payload = $payload ^ $mask;
    return $payload;
}

sub websocketPong {
    my $hash = shift;
    my $name = $hash->{NAME};
    Log3($name, 4, "websocketPong");
    websocketEncode($hash, undef, "pong");
}

#######################################
#       MQTT Functions
#######################################

sub mqttDecode {
    my ($hash, $string) = @_;
    my $name = $hash->{NAME};

    # JSON Decode und Fehlerüberprüfung
    Log3($name, 5, "mqttDecode - String Hex: " . unpack('H*', $string));
    my $unicodeDecodedString = RHASSPY::unicodeDecode($string);
    Log3($name, 5, "mqttDecode - unicodeDecodedString Hex: " . unpack('H*', $unicodeDecodedString));
    my $utf8EncodedString = encode_utf8($unicodeDecodedString);
    Log3($name, 5, "mqttDecode - utf8EncodedString Hex: " . unpack('H*', $utf8EncodedString));
    Log3($name, 5, "mqttDecode - Decoded string: $utf8EncodedString");

    my $decoded = eval { decode_json($utf8EncodedString) };
    if ($@) {
        Log3($name, 1, "mqttDecode - JSON decoding error: " . $@);
        return undef;
    }

    my $topic = $decoded->{'topic'};
    my $payload = $decoded->{'payload'};
    my $message = encode_json($payload);

    RHASSPY::onmessage($hash, $topic, $message);
}

sub mqttPublish {
    my ($hash, $topic, $message) = @_;

    my $sendData =  {
        type => "publish",
        topic => $topic,
        payload => $message
    };

    my $json = encode_json($sendData);
    RHASSPY::websocketEncode($hash, $json);
}

sub mqttSubscribe {
    my ($hash, $topic) = @_;

    my $sendData =  {
        type => "subscribe",
        topic => $topic
    };

    my $json = encode_json($sendData);
    RHASSPY::websocketEncode($hash, $json);
}

sub mqttSubscribeTopics {
    my ($hash) = @_;

    foreach (@topics) {
        my $topic = $_;
        RHASSPY::mqttSubscribe($hash, $topic);

        Log3($hash->{NAME}, 5, "mqttSubscribeTopics - Topic subscribed: " . $topic);
    }
}

1;
