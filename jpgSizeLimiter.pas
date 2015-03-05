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
  if paramcount>=2 then begin
    if      paramCount=2 then outputName:=paramstr(1)
    else if paramCount=3 then outputName:=paramstr(3);
    sizeLimit:=sizeToInt(paramstr(2));
    writeln('shrinking ',paramstr(1),' to ',outputName);
    writeln('size limit is ',sizelimit,'bytes = ',sizeLimit shr 10,'kB = ', sizeLimit shr 20,'MB');
    newFileSize:=filesize(paramstr(1));
    if (newFileSize < sizeLimit) and (uppercase(ExtractFileExt(paramstr(1)))='.JPG') then begin
      writeln('size of input file is ',newFileSize,'bytes = ',newFileSize shr 10,'kB = ',newFileSize shr 20,'MB');
      writeln('recompression skipped');
      if paramstr(1)<>outputName then CopyFile(paramstr(1),outputName,true);
    end else begin
      convertFileWithSizeLimit(paramstr(1),outputName,sizeLimit);
    end;
  end;
end.