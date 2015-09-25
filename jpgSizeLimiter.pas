PROGRAM jpgSizeLimiter;
USES mypics,fileutil,sysutils;
VAR sizeLimit,newFileSize:longint;
    outputName:string;
FUNCTION sizeToInt(s:string):longint;
  begin
    s:=trim(s);
    case s[length(s)] of
      'k','K': result:=strToInt(copy(s,1,length(s)-1)) shl 10;
      'm','M': result:=strToInt(copy(s,1,length(s)-1)) shl 20;
      'g','G': result:=strToInt(copy(s,1,length(s)-1)) shl 30;
    end;
  end;


begin
  if paramCount>=2 then begin
    if      paramCount=2 then outputName:=paramStr(1)
    else if paramCount=3 then outputName:=paramStr(3);
    sizeLimit:=sizeToInt(paramStr(2));
    writeln('shrinking ',paramStr(1),' to ',outputName);
    writeln('size limit is ',sizelimit,'bytes = ',sizeLimit shr 10,'kB = ', sizeLimit shr 20,'MB');
    newFileSize:=filesize(paramStr(1));
    if (newFileSize < sizeLimit) and (uppercase(extractFileExt(paramStr(1)))='.JPG') then begin
      writeln('size of input file is ',newFileSize,'bytes = ',newFileSize shr 10,'kB = ',newFileSize shr 20,'MB');
      writeln('recompression skipped');
      if paramStr(1)<>outputName then CopyFile(paramStr(1),outputName,true);
    end else begin
      convertFileWithSizeLimit(paramStr(1),outputName,sizeLimit);
    end;
  end;
end.
