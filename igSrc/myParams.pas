UNIT myParams;
INTERFACE
USES myColors,myGenerics,math,myStringUtil,sysutils;
CONST
  IMAGE_TYPE_EXTENSIONS :array[0..4] of string=('.JPG','.JPEG','.PNG','.BMP','.VRAW');
TYPE
  T_parameterType=(pt_none,
                   pt_string,
                   pt_fileName,
                   pt_jpgNameWithSize,
                   pt_enum,
                   pt_integer,
                   pt_2integers,
                   pt_4integers,
                   pt_color,
                   pt_float,
                   pt_2floats,
                   pt_floatOr2Floats,
                   pt_3floats);

  P_parameterDescription=^T_parameterDescription;
  T_parameterDescription=object
    name:string;
    typ :T_parameterType;
    minValue,maxValue:double;
    enumValues: T_arrayOfString;
    CONSTRUCTOR create(CONST name_: string;
                       CONST typ_: T_parameterType;
                       CONST minValue_: double= -infinity;
                       CONST maxValue_: double=  infinity;
                       CONST eT00: ansistring=''; CONST eT01: ansistring=''; CONST eT02: ansistring='';
                       CONST eT03: ansistring=''; CONST eT04: ansistring=''; CONST eT05: ansistring='';
                       CONST eT06: ansistring=''; CONST eT07: ansistring=''; CONST eT08: ansistring='';
                       CONST eT09: ansistring=''; CONST eT10: ansistring=''; CONST eT11: ansistring='';
                       CONST eT12: ansistring=''; CONST eT13: ansistring=''; CONST eT14: ansistring='';
                       CONST eT15: ansistring='');
    DESTRUCTOR destroy;
    FUNCTION shortName:string;
    FUNCTION describe:ansistring;
  end;

  T_parameterNameMode=(tsm_withoutParameterName,
                       tsm_withNiceParameterName,
                       tsm_forSerialization);

  { T_parameterValue }

  T_parameterValue=object
    private
      associatedParmeterDescription:P_parameterDescription;
      fileNameValue:string;
      intValue:array[0..3] of longint;
      floatValue:array[0..2] of double;
      valid:boolean;
    public
      CONSTRUCTOR createToParse  (CONST parameterDescription:P_parameterDescription; CONST stringToParse:ansistring; CONST parameterNameMode:T_parameterNameMode=tsm_withoutParameterName);
      CONSTRUCTOR createFromValue(CONST parameterDescription:P_parameterDescription; CONST i0:longint; CONST i1:longint=0; CONST i2:longint=0; CONST i3:longint=0);
      CONSTRUCTOR createFromValue(CONST parameterDescription:P_parameterDescription; CONST f0:double; CONST f1:double=0; CONST f2:double=0);
      CONSTRUCTOR createFromValue(CONST parameterDescription:P_parameterDescription; CONST color:T_floatColor);
      CONSTRUCTOR createFromValue(CONST parameterDescription:P_parameterDescription; CONST txt:ansistring; CONST sizeLimit:longint=-1);
      FUNCTION canParse(CONST stringToParse:ansistring; CONST parameterNameMode:T_parameterNameMode=tsm_withoutParameterName):boolean;
      FUNCTION isValid:boolean;
      FUNCTION toString(CONST parameterNameMode:T_parameterNameMode=tsm_withoutParameterName):ansistring;

      FUNCTION fileName:string;
      FUNCTION i0:longint;
      FUNCTION i1:longint;
      FUNCTION i2:longint;
      FUNCTION i3:longint;
      FUNCTION f0:double;
      FUNCTION f1:double;
      FUNCTION f2:double;
      FUNCTION color:T_floatColor;
      FUNCTION strEq(CONST other:T_parameterValue):boolean;
  end;

IMPLEMENTATION
FUNCTION parameterDescription(CONST name:string; CONST typ:T_parameterType; CONST minValue:double=-infinity; CONST maxValue:double=infinity):T_parameterDescription;
  begin
    result.name:=name;
    result.typ:=typ;
    result.minValue:=minValue;
    result.maxValue:=maxValue;
    setLength(result.enumValues,0);
  end;

FUNCTION T_parameterDescription.shortName:string;
  begin
    result:=replaceAll(cleanString(name,IDENTIFIER_CHARS,' '),' ','');
  end;

FUNCTION T_parameterDescription.describe:ansistring;
  VAR i:longint;
  begin
    result:='name: "'+name+'"; parameters: ';
    case typ of
      pt_none: result:=result+'none';
      pt_string: result:=result+' string';
      pt_fileName: result:=result+'file name (allowed extensions: .jpg .png .bmp .vraw)';
      pt_jpgNameWithSize: result:=result+'fileName.jpg@size (e.g. test.jpg@1M)';
      pt_enum: begin result:=result+'enum';
        result:=result+C_lineBreakChar+'Values:';
        for i:=0 to length(enumValues)-1 do result:=result+C_lineBreakChar+enumValues[i];
      end;
      pt_integer: result:=result+'integer';
      pt_2integers: result:=result+'x,y (two integers)';
      pt_4integers: result:=result+'x0,x1,y0,y1 (four integers)';
      pt_color: result:=result+'color (as RGB, e.g. red: 1,0,0)';
      pt_float: result:=result+'float';
      pt_2floats: result:=result+'x,y (two floats)';
      pt_floatOr2Floats: result:=result+'x,y (one or two floats)';
      pt_3floats: result:=result+'x,y,z (three floats)';
    end;
  end;

VAR PARAMETER_SPLITTERS:T_arrayOfString;

{ T_parameterDescription }

CONSTRUCTOR T_parameterDescription.create(CONST name_: string;
  CONST typ_: T_parameterType; CONST minValue_: double;
  CONST maxValue_: double; CONST eT00: ansistring; CONST eT01: ansistring;
  CONST eT02: ansistring; CONST eT03: ansistring; CONST eT04: ansistring;
  CONST eT05: ansistring; CONST eT06: ansistring; CONST eT07: ansistring;
  CONST eT08: ansistring; CONST eT09: ansistring; CONST eT10: ansistring;
  CONST eT11: ansistring; CONST eT12: ansistring; CONST eT13: ansistring;
  CONST eT14: ansistring; CONST eT15: ansistring);
  begin
    name:=name_;
    typ:=typ_;
    minValue:=minValue_;
    maxValue:=maxValue_;
    setLength(enumValues,0);
    if eT00<>'' then append(enumValues,eT00);
    if eT01<>'' then append(enumValues,eT01);
    if eT02<>'' then append(enumValues,eT02);
    if eT03<>'' then append(enumValues,eT03);
    if eT04<>'' then append(enumValues,eT04);
    if eT05<>'' then append(enumValues,eT05);
    if eT06<>'' then append(enumValues,eT06);
    if eT07<>'' then append(enumValues,eT07);
    if eT08<>'' then append(enumValues,eT08);
    if eT09<>'' then append(enumValues,eT09);
    if eT10<>'' then append(enumValues,eT10);
    if eT11<>'' then append(enumValues,eT11);
    if eT12<>'' then append(enumValues,eT12);
    if eT13<>'' then append(enumValues,eT13);
    if eT14<>'' then append(enumValues,eT14);
    if eT15<>'' then append(enumValues,eT15);
  end;

DESTRUCTOR T_parameterDescription.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(enumValues)-1 do enumValues[i]:='';
    setLength(enumValues,0);
  end;

{ T_parameterValue }

CONSTRUCTOR T_parameterValue.createToParse(CONST parameterDescription: P_parameterDescription; CONST stringToParse: ansistring; CONST parameterNameMode:T_parameterNameMode=tsm_withoutParameterName);
  begin
    associatedParmeterDescription:=parameterDescription;
    canParse(stringToParse,parameterNameMode);
  end;

FUNCTION T_parameterValue.canParse(CONST stringToParse:ansistring; CONST parameterNameMode:T_parameterNameMode=tsm_withoutParameterName):boolean;
  VAR txt:string;
      part:T_arrayOfString;
      i:longint;
      expectedPrefix:string;
  begin
    valid:=false;
    txt:=trim(stringToParse);
    case parameterNameMode of
      tsm_forSerialization:      expectedPrefix:=associatedParmeterDescription^.shortName;
      tsm_withNiceParameterName: expectedPrefix:=associatedParmeterDescription^.name;
      else expectedPrefix:='';
    end;
    if (associatedParmeterDescription^.typ<>pt_none) and (expectedPrefix<>'') then expectedPrefix:=expectedPrefix+':';
    if not(startsWith(txt,expectedPrefix)) then begin
      valid:=false;
      exit(valid);
    end;
    txt:=trim(copy(txt,length(expectedPrefix)+1,length(txt)));
    case associatedParmeterDescription^.typ of
      pt_none: valid:=txt='';
      pt_string: begin
        valid:=txt<>'';
        fileNameValue:=txt;
      end;
      pt_fileName: begin
        if (copy(txt,1,1)=':') then txt:=copy(txt,2,length(txt)-1);
        fileNameValue:=txt;
        valid:=isFilename(txt,IMAGE_TYPE_EXTENSIONS);
      end;
      pt_jpgNameWithSize: begin
        part:=split(txt,'@');
        fileNameValue:=part[0];
        if not(isFilename(fileName,T_arrayOfString('.JPG'))) then begin valid:=false; exit(valid); end;
        if length(part)<>2 then begin valid:=false; exit(valid); end else txt:=part[1];
        if      endsWith(uppercase(txt),'K') then i:=1 shl 10
        else if endsWith(uppercase(txt),'M') then i:=1 shl 20
        else i:=1;
        if i>1 then txt:=copy(txt,1,length(txt)-1);
        try
          intValue[0]:=i*strToInt(txt);
        except
          begin valid:=false; exit(valid); end;
        end;
        valid:=intValue[0]>0;
      end;

      pt_enum: begin
        valid:=false;
        for i:=0 to length(associatedParmeterDescription^.enumValues)-1 do if txt=trim(associatedParmeterDescription^.enumValues[i]) then begin
          intValue[0]:=i;
          fileNameValue:=txt;
          valid:=true;
          exit(valid);
        end;
        try
          valid:=true;
          intValue[0]:=strToInt(txt);
        except
          begin valid:=false; exit(valid); end;
        end;
        if (intValue[0]>=0) and (intValue[0]<length(associatedParmeterDescription^.enumValues)) then begin
          fileNameValue:=associatedParmeterDescription^.enumValues[intValue[0]];
          valid:=true;
        end else valid:=false;
      end;
      pt_integer: begin
        try
          valid:=true;
          intValue[0]:=strToInt(txt);
        except
          begin valid:=false; exit(valid); end;
        end;
        valid:=(intValue[0]>=associatedParmeterDescription^.minValue)
           and (intValue[0]<=associatedParmeterDescription^.maxValue);
      end;
      pt_float: begin
        try
          valid:=true;
          floatValue[0]:=strToFloat(txt);
        except
          begin valid:=false; exit(valid); end;
        end;
        valid:=(floatValue[0]>=associatedParmeterDescription^.minValue)
           and (floatValue[0]<=associatedParmeterDescription^.maxValue);
      end;
      pt_2integers,pt_4integers,pt_color,pt_2floats,pt_floatOr2Floats,pt_3floats: begin
        part:=split(txt,PARAMETER_SPLITTERS);
        if not((length(part)=1) and (associatedParmeterDescription^.typ=pt_floatOr2Floats)
            or (length(part)=2) and (associatedParmeterDescription^.typ in [pt_2integers,pt_2floats,pt_floatOr2Floats])
            or (length(part)=3) and (associatedParmeterDescription^.typ in [pt_color,pt_3floats])
            or (length(part)=4) and (associatedParmeterDescription^.typ=pt_4integers)) then begin valid:=false; exit(valid); end;
        valid:=false;
        for i:=0 to length(part)-1 do if associatedParmeterDescription^.typ in [pt_2integers,pt_4integers] then begin
          try
            intValue[i]:=strToInt(part[i]);
            if (intValue[i]<associatedParmeterDescription^.minValue) or
               (intValue[i]>associatedParmeterDescription^.maxValue) then begin valid:=false; exit(valid); end;
          except
            begin valid:=false; exit(valid); end;
          end;
        end else begin
          try
            floatValue[i]:=strToFloat(part[i]);
            if (floatValue[i]<associatedParmeterDescription^.minValue) or
               (floatValue[i]>associatedParmeterDescription^.maxValue) then begin valid:=false; exit(valid); end;
          except
            begin valid:=false; exit(valid); end;
          end;
        end;
        if (length(part)=1) and (associatedParmeterDescription^.typ=pt_floatOr2Floats) then floatValue[1]:=floatValue[0];
        valid:=true;
      end;
    end;
    result:=valid;
  end;
CONSTRUCTOR T_parameterValue.createFromValue(CONST parameterDescription: P_parameterDescription; CONST i0: longint; CONST i1: longint; CONST i2: longint; CONST i3: longint);
  begin
    associatedParmeterDescription:=parameterDescription;
    intValue[0]:=i0;
    intValue[1]:=i1;
    intValue[2]:=i2;
    intValue[3]:=i3;
    if parameterDescription^.typ=pt_enum then fileNameValue:=parameterDescription^.enumValues[i0];
  end;

CONSTRUCTOR T_parameterValue.createFromValue(CONST parameterDescription: P_parameterDescription; CONST f0: double; CONST f1: double; CONST f2: double);
  begin
    associatedParmeterDescription:=parameterDescription;
    floatValue[0]:=f0;
    floatValue[1]:=f1;
    floatValue[2]:=f2;
  end;

CONSTRUCTOR T_parameterValue.createFromValue(CONST parameterDescription: P_parameterDescription; CONST color: T_floatColor);
  begin
    associatedParmeterDescription:=parameterDescription;
    floatValue[0]:=color[0];
    floatValue[1]:=color[1];
    floatValue[2]:=color[2];
  end;

CONSTRUCTOR T_parameterValue.createFromValue(CONST parameterDescription:P_parameterDescription; CONST txt:ansistring; CONST sizeLimit:longint=-1);
  begin
    associatedParmeterDescription:=parameterDescription;
    fileNameValue:=txt;
    intValue[0]:=sizeLimit;
  end;

FUNCTION T_parameterValue.isValid: boolean;
  begin
    result:=valid;
  end;

FUNCTION T_parameterValue.toString(CONST parameterNameMode:T_parameterNameMode=tsm_withoutParameterName): ansistring;
  begin
    case parameterNameMode of
      tsm_forSerialization:      result:=associatedParmeterDescription^.shortName;
      tsm_withNiceParameterName: result:=associatedParmeterDescription^.name;
      tsm_withoutParameterName:  result:='';
    end;
    if (associatedParmeterDescription^.typ<>pt_none) and (result<>'') then result:=result+':';

    case associatedParmeterDescription^.typ of
      pt_fileName,pt_string,pt_enum: result:=result+fileName;
      pt_jpgNameWithSize:  result:=result+fileName+'@'+intToStr(intValue[0]);
      pt_integer:          result:=result+intToStr(intValue[0]);
      pt_2integers:        result:=result+intToStr(intValue[0])+
                                          ','+intToStr(intValue[1]);
      pt_4integers:        result:=result+intToStr(intValue[0])+
                                          ':'+intToStr(intValue[1])+
                                          'x'+intToStr(intValue[2])+
                                          ':'+intToStr(intValue[3]);
      pt_float:            result:=result+floatToStr(floatValue[0]);
      pt_2floats:          result:=result+floatToStr(floatValue[0])+
                                          ','+floatToStr(floatValue[1]);
      pt_3floats,pt_color: result:=result+floatToStr(floatValue[0])+
                                          ','+floatToStr(floatValue[1])+
                                          ','+floatToStr(floatValue[2]);
      pt_floatOr2Floats: begin
        result:=result+floatToStr(floatValue[0]);
        if floatValue[1]<>floatValue[0] then result:=result+','+floatToStr(floatValue[1]);
      end;
    end;
  end;

FUNCTION T_parameterValue.fileName: string; begin result:=fileNameValue; end;
FUNCTION T_parameterValue.i0: longint; begin result:=intValue[0]; end;
FUNCTION T_parameterValue.i1: longint; begin result:=intValue[1]; end;
FUNCTION T_parameterValue.i2: longint; begin result:=intValue[2]; end;
FUNCTION T_parameterValue.i3: longint; begin result:=intValue[3]; end;
FUNCTION T_parameterValue.f0: double; begin result:=floatValue[0]; end;
FUNCTION T_parameterValue.f1: double; begin result:=floatValue[1]; end;
FUNCTION T_parameterValue.f2: double; begin result:=floatValue[2]; end;
FUNCTION T_parameterValue.color: T_floatColor;
  begin
    result:=newColor(floatValue[0],floatValue[1],floatValue[2]);
  end;
FUNCTION T_parameterValue.strEq(CONST other:T_parameterValue):boolean;
  begin
    result:=toString(tsm_forSerialization)=other.toString(tsm_forSerialization);
  end;

INITIALIZATION
  PARAMETER_SPLITTERS:=',';
  append(PARAMETER_SPLITTERS,':');
  append(PARAMETER_SPLITTERS,'x');
  DefaultFormatSettings.DecimalSeparator:='.';

end.
