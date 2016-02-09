UNIT myParams;
INTERFACE
USES myColors,myGenerics,math,myStringUtil,sysutils;
CONST
  IMAGE_TYPE_EXTENSIONS :array[0..4] of string=('.JPG','.JPEG','.PNG','.BMP','.VRAW');
TYPE
  T_parameterType=(pt_none,
                   pt_fileName,
                   pt_jpgNameWithSize,
                   pt_enum,
                   pt_integer,
                   pt_2integers,
                   pt_4integers,
                   pt_color,
                   pt_float,
                   pt_3floats);

  T_parameterValue=record
    fileName:string;
    intValue:array[0..3] of longint;
    floatValue:array[0..2] of double;
    color:T_floatColor;
  end;

  T_simplifiedParameterDescription=record
    name:string;
    typ :T_parameterType;
    minValue,maxValue:double;
  end;

  T_parameterDescription=record
    name:string;
    typ :T_parameterType;
    minValue,maxValue:double;
    enumValues: T_arrayOfString;
  end;

FUNCTION parameterDescription(CONST name:string; CONST typ:T_parameterType; CONST minValue:double=-infinity; CONST maxValue:double=infinity):T_parameterDescription;
FUNCTION canParseParameterValue(CONST parameterDescription:T_parameterDescription; CONST stringToParse:string; OUT parameterValue:T_parameterValue; CONST parameterNameIncluded:boolean=false):boolean;
FUNCTION toString(CONST parameterDescription:T_parameterDescription; CONST parameterValue:T_parameterValue; CONST parameterNameIncluded:boolean=false):string;
OPERATOR :=(CONST x:T_simplifiedParameterDescription):T_parameterDescription;
IMPLEMENTATION
FUNCTION parameterDescription(CONST name:string; CONST typ:T_parameterType; CONST minValue:double=-infinity; CONST maxValue:double=infinity):T_parameterDescription;
  begin
    result.name:=name;
    result.typ:=typ;
    result.minValue:=minValue;
    result.maxValue:=maxValue;
    setLength(result.enumValues,0);
  end;

VAR PARAMETER_SPLITTERS:T_arrayOfString;
FUNCTION canParseParameterValue(CONST parameterDescription:T_parameterDescription; CONST stringToParse:string; OUT parameterValue:T_parameterValue; CONST parameterNameIncluded:boolean=false):boolean;
  VAR txt:string;
      part:T_arrayOfString;
      i:longint;
  begin
    result:=false;
    txt:=trim(stringToParse);
    if parameterNameIncluded then begin
      if not(startsWith(txt,parameterDescription.name))
      then exit(false)
      else txt:=trim(copy(txt,length(parameterDescription.name),length(txt)));
    end;
    case parameterDescription.typ of
      pt_none: result:=txt='';
      pt_fileName: begin
        if (copy(txt,1,1)=':') then txt:=copy(txt,2,length(txt)-1);
        parameterValue.fileName:=txt;
        result:=isFilename(txt,IMAGE_TYPE_EXTENSIONS);
      end;
      pt_jpgNameWithSize: begin
        part:=split(txt,'@');
        parameterValue.fileName:=part[0];
        if not(isFilename(parameterValue.fileName,T_arrayOfString('.JPG'))) then exit(false);
        if length(part)<>2 then exit(false) else txt:=part[1];
        if      endsWith(uppercase(txt),'K') then i:=2 shl 10
        else if endsWith(uppercase(txt),'M') then i:=2 shl 20
        else i:=1;
        if i>1 then txt:=copy(txt,1,length(txt)-1);
        try
          parameterValue.intValue[0]:=i*strToInt(txt);
        except
          exit(false);
        end;
        result:=parameterValue.intValue[0]>0;
      end;

      pt_enum: begin
        result:=false;
        for i:=0 to length(parameterDescription.enumValues)-1 do if txt=trim(parameterDescription.enumValues[i]) then begin
          result:=true;
          parameterValue.intValue[0]:=i;
          parameterValue.fileName:=txt;
        end;
      end;
      pt_integer: begin
        try
          result:=true;
          parameterValue.intValue[0]:=strToInt(txt);
        except
          exit(false);
        end;
        result:=(parameterValue.intValue[0]>=parameterDescription.minValue)
            and (parameterValue.intValue[0]<=parameterDescription.maxValue);
      end;
      pt_float: begin
        try
          result:=true;
          parameterValue.floatValue[0]:=strToFloat(txt);
        except
          exit(false);
        end;
        result:=(parameterValue.floatValue[0]>=parameterDescription.minValue)
            and (parameterValue.floatValue[0]<=parameterDescription.maxValue);
      end;
      pt_2integers,pt_4integers,pt_color,pt_3floats: begin
        part:=split(txt,PARAMETER_SPLITTERS);
        if not((length(part)=2) and (parameterDescription.typ=pt_2integers)
            or (length(part)=3) and (parameterDescription.typ in [pt_color,pt_3floats])
            or (length(part)=4) and (parameterDescription.typ=pt_4integers)) then exit(false);
        result:=false;
        for i:=0 to length(part)-1 do if parameterDescription.typ in [pt_2integers,pt_4integers] then begin
          try
            parameterValue.intValue[i]:=strToInt(part[i]);
            if (parameterValue.intValue[i]<parameterDescription.minValue) or
               (parameterValue.intValue[i]>parameterDescription.maxValue) then exit(false);
          except
            exit(false);
          end;
        end else begin
          try
            parameterValue.floatValue[i]:=strToFloat(part[i]);
            if i<3 then parameterValue.color[i]:=parameterValue.floatValue[i];
            if (parameterValue.floatValue[i]<parameterDescription.minValue) or
               (parameterValue.floatValue[i]>parameterDescription.maxValue) then exit(false);
          except
            exit(false);
          end;
        end;
        result:=true;
      end;
    end;
  end;

FUNCTION toString(CONST parameterDescription:T_parameterDescription; CONST parameterValue:T_parameterValue; CONST parameterNameIncluded:boolean=false):string;
  begin
    if parameterNameIncluded then result:=parameterDescription.name
                             else result:='';
    case parameterDescription.typ of
      pt_fileName,pt_enum: result:=result+parameterValue.fileName;
      pt_jpgNameWithSize:  result:=result+parameterValue.fileName+'@'+IntToStr(parameterValue.intValue[0]);
      pt_integer:          result:=result+IntToStr(parameterValue.intValue[0]);
      pt_2integers:        result:=result+IntToStr(parameterValue.intValue[0])+
                                      ','+IntToStr(parameterValue.intValue[1]);
      pt_4integers:        result:=result+IntToStr(parameterValue.intValue[0])+
                                      ':'+IntToStr(parameterValue.intValue[1])+
                                      'x'+IntToStr(parameterValue.intValue[2])+
                                      ':'+IntToStr(parameterValue.intValue[3]);
      pt_float:            result:=result+FloatToStr(parameterValue.floatValue[0]);
      pt_color,pt_3floats: result:=result+FloatToStr(parameterValue.floatValue[0])+
                                      ','+FloatToStr(parameterValue.floatValue[1])+
                                      ','+FloatToStr(parameterValue.floatValue[2]);
    end;
  end;

OPERATOR := (CONST x: T_simplifiedParameterDescription): T_parameterDescription;
  begin
    result.name      :=x.name    ;
    result.typ       :=x.typ     ;
    result.minValue  :=x.minValue;
    result.maxValue  :=x.maxValue;
    result.enumValues:=C_EMPTY_STRING_ARRAY;
  end;


INITIALIZATION
  PARAMETER_SPLITTERS:=',';
  append(PARAMETER_SPLITTERS,';');
  append(PARAMETER_SPLITTERS,':');
  append(PARAMETER_SPLITTERS,'x');
  DecimalSeparator:='.';

end.
