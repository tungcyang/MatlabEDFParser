function [header, recordData] = EDFparser(fileName)
% Read European Data Format (EDF) file into MATLAB
%
% Please refer to "Full specification of EDF" in http://www.edfplus.info/specs/edf.html
%
% [header, record] = EDFparser(fname)
%         Reads data from ALL RECORDS of file fname ('*.edf'). Header
%         information is returned in structure header, and the signals
%         (waveforms) are returned in cell array recordData.

% Constants:
    InvalidFileIdentifier = -1;
    numHeaderBytesVer = 8;
    numHeaderBytesPatientID = 80;
    numHeaderBytesRecordID = 80;
    numHeaderBytesStartDate = 8;
    numHeaderBytesStartTime = 8;
    numHeaderBytesBytes = 8;
    numHeaderBytesReserved = 44;
    numHeaderBytesRecords = 8;
    numHeaderBytesDuration = 8;
    numHeaderBytesNumSignals = 4;
    numHeaderBytesLabels = 16;
    numHeaderBytesTransducerTypes = 80;
    numHeaderBytesPhysicalDim = 8;
    numHeaderBytesPhysicalMin = 8;
    numHeaderBytesPhysicalMax = 8;
    numHeaderBytesDigitalMin = 8;
    numHeaderBytesDigitalMax = 8;
    numHeaderBytesPrefilter = 80;
    numHeaderBytesSamples = 8;
    numHeaderBytesNSReserved = 32;
    
    [fEDFId, message] = fopen(fileName, 'r');
    if fEDFId == InvalidFileIdentifier
        % Note that message is system-dependent.
        error(message);
        return;
    end
    
    % Parsing header as described in "Full specification of EDF"
    
    % HEADER RECORD (we suggest to also adopt the 12 simple additional EDF+ specs)
    % 8 ascii : version of this data format (0)
    header.ver = str2double(fread(fEDFId, numHeaderBytesVer, '*char')');   % It seems fread() is expected to return '0       ';
    % 80 ascii : local patient identification.
    % The 'local patient identification' field must start with the subfields
    % (subfields do not contain, but are separated by, spaces): 
    %     - the code by which the patient is known in the hospital administration. 
    %     - sex (English, so F or M). 
    %     - birthdate in dd-MMM-yyyy format using the English 3-character
    %       abbreviations of the month in capitals. 02-AUG-1951 is OK, while
    %       2-AUG-1951 is not. 
    %     - the patients name. 
    % Any space inside the hospital code or the name of the patient must be
    % replaced by a different character, for instance an underscore.  For
    % instance, the 'local patient identification' field could start with:
    % MCH-0234567 F 02-MAY-1951 Haagse_Harry.  Subfields whose contents are
    % unknown, not applicable or must be made anonymous are replaced by a
    % single character 'X'. Additional subfields may follow the ones described here. 
    header.patientID  = fread(fEDFId, numHeaderBytesPatientID, '*char')';
    % 80 ascii : local recording identification.  The 'local recording identification'
    % field must start with the subfields (subfields do not contain, but are
    % separated by, spaces): 
    %     - The text 'Startdate'. 
    %     - The startdate itself in dd-MMM-yyyy format using the English
    %       3-character abbreviations of the month in capitals. 
    %     - The hospital administration code of the investigation, i.e. EEG
    %       number or PSG number. 
    %     - A code specifying the responsible investigator or technician. 
    %     - A code specifying the used equipment. 
    % Any space inside any of these codes must be replaced by a different
    % character, for instance an underscore.  The 'local recording identification'
    % field could contain: Startdate 02-MAR-2002 PSG-1234/2002 NN Telemetry03.
    % Subfields whose contents are unknown, not applicable or must be made
    % anonymous are replaced by a single character 'X'.  So, if everything is
    % unknown then the 'local recording identification' field would start with:
    % Startdate X X X X. Additional subfields may follow the ones described here. 
    header.recordID   = fread(fEDFId, numHeaderBytesRecordID, '*char')';
    % 8 ascii : startdate of recording (dd.mm.yy).  1985 is the clipping
    % date to avoid Y2K problems, so yy in 85-99 means from 1985 to 1999,
    % and yy in 00-84 means from 2000 to 2084.
    header.startdate  = fread(fEDFId, numHeaderBytesStartDate, '*char')';     % (dd.mm.yy)
    % 8 ascii : starttime of recording (hh.mm.ss) 
    header.starttime  = fread(fEDFId, numHeaderBytesStartTime, '*char')';     % (hh.mm.ss)
    % 8 ascii : number of bytes in header record
    header.bytes      = str2double(fread(fEDFId, numHeaderBytesBytes, '*char')');
    if header.bytes <= 0
        disp('ERROR: Number of header bytes not positive!');
        return;
    end

    % At this point we have already read (8 + 80 + 80 + 8 + 8 + 8) = 192
    % bytes.

    % 44 ascii : reserved
    fread(fEDFId, numHeaderBytesReserved);
    % 8 ascii : number of data records (-1 if unknown, obey item 10 of the additional EDF+ specs)
    header.records    = str2double(fread(fEDFId, numHeaderBytesRecords, '*char')');
    if header.records == -1
        disp('ERROR: EDF spec restricts number of data records at -1 only during recording!');
        return;
    end
    % 8 ascii : duration of a data record, in seconds
    header.duration   = str2double(fread(fEDFId, numHeaderBytesDuration, '*char')');
    if header.duration <= 0
        disp('ERROR: Duration (seconds) not positive!');
        return;
    end
    % is recommended not to exceed 61440
    if header.duration > 61440
        disp('WARNING: Duration (seconds) exceeds 61440, which is not recommended!');
    end
    % 4 ascii : number of signals (ns) in data record
    header.ns         = str2double(fread(fEDFId, numHeaderBytesNumSignals, '*char')');
    if header.ns <= 0
        disp('ERROR: Number of Signals (ns) not positive!');
        return;
    end
    
    % At this point we have already read 192 + 44 + 8 + 8 + 4 = 256 bytes.

    % ns * 16 ascii : ns * label (e.g. EEG Fpz-Cz or Body temp).  All white
    % spaces will be removed.
    for i = 1:header.ns
        % Note that in EDF+ data in this field need to follow the standard
        % texts and polarity rules (http://www.edfplus.info/specs/edftexts.html).
        header.label{i} = regexprep(fread(fEDFId, numHeaderBytesLabels, '*char')', '\W', '');
    end
    % ns * 80 ascii : ns * transducer type (e.g. AgAgCl electrode)
    for i = 1:header.ns
        header.transducer{i} = fread(fEDFId, numHeaderBytesTransducerTypes, '*char')';
    end
    % ns * 8 ascii : ns * physical dimension (e.g. uV or degreeC)
    for i = 1:header.ns
        header.units{i} = fread(fEDFId, numHeaderBytesPhysicalDim, '*char')';
    end
    % ns * 8 ascii : ns * physical minimum (e.g. -500 or 34)
    for i = 1:header.ns
        header.physicalMin(i) = str2double(fread(fEDFId, numHeaderBytesPhysicalMin, '*char')');
    end
    % ns * 8 ascii : ns * physical maximum (e.g. 500 or 40)
    for i = 1:header.ns
        header.physicalMax(i) = str2double(fread(fEDFId, numHeaderBytesPhysicalMax, '*char')');
        if header.physicalMax(i) < header.physicalMin(i)
            disp(['ERROR: For the ' num2str(i) 'th signal physical max ' num2str(header.physicalMax(i)) ...
                  ' is smaller than the physical min ' num2str(header.physicalMin(i)) '!']);
            return;
        end
    end
    % ns * 8 ascii : ns * digital minimum (e.g. -2048)
    for i = 1:header.ns
        header.digitalMin(i) = str2double(fread(fEDFId, numHeaderBytesDigitalMin, '*char')');
    end
    % ns * 8 ascii : ns * digital maximum (e.g. 2047)
    for i = 1:header.ns
        header.digitalMax(i) = str2double(fread(fEDFId, numHeaderBytesDigitalMax, '*char')');
        if header.digitalMax(i) < header.digitalMin(i)
            disp(['ERROR: For the ' num2str(i) 'th signal digital max ' num2str(header.digitalMax(i)) ...
                  ' is smaller than the digital min ' num2str(header.digitalMin(i)) '!']);
            return;
        end
    end
    % ns * 80 ascii : ns * prefiltering (e.g. HP:0.1Hz LP:75Hz)
    for i = 1:header.ns
        header.prefilter{i} = fread(fEDFId, numHeaderBytesPrefilter, '*char')';
    end
    % ns * 8 ascii : ns * nr of samples in each data record
    for i = 1:header.ns
        header.samples(i) = str2double(fread(fEDFId, numHeaderBytesSamples, '*char')');
    end
    % ns * 32 ascii : ns * reserved
    for i = 1:header.ns
        fread(fEDFId, numHeaderBytesNSReserved, '*char');
    end
            
    % At this point we have already read
    % 256 + header.ns*(16 + 80 + 8 + 8 + 8 + 8 + 8 + 80 + 8 + 32) =
    % 256 + header.ns*256 bytes.
    if header.bytes ~= (256 + header.ns*256)
        disp('ERROR: Number of header bytes not consistent!');
        return;
    end
    
    % Now parsing the data records.
    % nr of samples[1] * integer : first signal in the data record 
    % nr of samples[2] * integer : second signal 
    %   ......
    %   ......
    % nr of samples[ns] * integer : last signal
    %
    % This is what the EDF spec on edfplus.info says.  However, in reality
    % it seems data are organized in such a way that:
    %   1. we have header.records rows of data;
    %   2. for each row we first have header.samples(1) data for signal 1,
    %      then header.samples(2) data for signal 2, etc.
    amplification = (header.physicalMax - header.physicalMin)./ ...
                    (header.digitalMax - header.digitalMin);
    offset = header.physicalMax - amplification.*header.digitalMax;
    % Both amplification and offset are vectors for the number of signals
    % (header.ns).

    % Preallocating recordData.
    recordData = cell(header.ns, 1);
    for i = 1:header.ns
        recordData{i} = zeros(1, header.records*header.samples(i));
    end
    recordDataLen = zeros(header.ns, 1);    % The length of data already
                                            % saved in record.data{}.
    recordTotalDataLen = sum(header.samples);
                                            
    for j = 1:header.records
        % Slurping in all the data corresponding to a record.
        % indexTempdata is used to address tempdata[].  Each sample value
        % is represented as a 2-byte integer in 2's complement format.
        tempdata = fread(fEDFId, recordTotalDataLen, 'int16=>double');
        indexTempdata = 1;
        for i = 1:header.ns
            % Chopping the data in tempdata into corresponding data
            % channels in record.
            recordData{i}((recordDataLen(i) + 1):(recordDataLen(i) + header.samples(i))) = ...
                tempdata(indexTempdata:(indexTempdata + header.samples(i) - 1));
            recordDataLen(i) = recordDataLen(i) + header.samples(i);
            indexTempdata = indexTempdata + header.samples(i);
        end
    end
    
    % Now performing the linear transform with amplification and offset.
    for i = 1:header.ns
        recordData{i, 1} = recordData{i, 1}*amplification(i) + offset(i);
    end

    % Job done.  Time to clean up.
    if fEDFId ~= InvalidFileIdentifier
        fclose(fEDFId);
    end