PROGRAM stupidPlot;
USES {$ifdef UNIX}cmem,cthreads,{$endif}mypics,sysutils,Process,math;

PROCEDURE backgroundDisplay(ps:string);
  VAR tempProcess:TProcess;
  begin
    tempProcess :=TProcess.Create(nil);
    tempProcess.CommandLine :={$ifdef UNIX}'./'+{$endif} 'display '+ps;
    tempProcess.execute;
    tempProcess.Free;
  end;

PROCEDURE parseCommandline;
  VAR xres,yres,ix,iy:longint;
      x0,y0,x1,y1:double;
      inputFileName:string;
      outputFileName:string;
      sep:string;
      validCmdLine:boolean;
      nicePlot:boolean;

  PROCEDURE generatePlot;
    VAR pic,avgPic:T_ByteMap;
        f:text;
        i,j,k,subsample:longint;
        nextline,nextItem:ansistring;
        sample:array of double;
        xScale,yScale:double;
        shiftX,shiftY:double;
        
        pt,apt:PByte;
    begin



      xScale:=xres/(x1-x0);
      yScale:=yres/(y1-y0);
      pic   .create(xres,yres);
      avgPic.create(xres,yres);
      avgPic.setToValue(0);
      for subsample:=1 to 17 do if (subsample=1) or nicePlot then begin
        pic.setToValue(0);
        shiftX:=0.5-random;
        shiftY:=0.5-random;
        setLength(sample,0);
        try
          assign(f,inputFileName);
          reset(f);
          i:=0;
          while not(eof(f)) do begin
            readln(f,nextLine);
            nextLine:=trim(nextline);
            j:=0;
            while length(nextLine)>0 do begin
              k:=pos(sep,nextLine);
              if k<=0 then begin
                nextItem:=trim(nextLine);
                nextLine:='';
              end else begin
                nextitem:=trim(copy(nextLine,1,k-1));
                nextLine:=trim(copy(nextLine,k+length(sep),length(nextLine)));
              end;
              if j>=length(sample) then setLength(sample,j+1);
              sample[j]:=strToFloatDef(nextItem,0);
              inc(j);
            end;
            for k:=0 to (j-1) div 2 do begin
              if (sample[k+k  ]>=x0) and (sample[k+k  ]<=x1) and
                 (sample[k+k+1]>=y0) and (sample[k+k+1]<=y1) then
              pic[round((sample[k+k  ]-x0)*xScale+0.5-shiftX),yres-1-round((sample[k+k+1]-y0)*yScale+0.5-shiftY)]:=15;
            end;
            inc(i);
          end;
          close(f);
        except
        end;
        pt :=   Pic.rawData;
        apt:=avgPic.rawData;
        if nicePlot then for i:=0 to pic.size-1 do apt[i]:=apt[i]+pt[i]
                    else for i:=0 to pic.size-1 do apt[i]:=apt[i]+pt[i]*17;
      end;
      avgPic.saveToFile(outputFileName);
      avgPic.destroy;
         pic.destroy;
      //backgroundDisplay(outputFileName);
    end;



  PROCEDURE displayHelp;
    begin
      writeln('List of command line parameters');
      writeln('  -h    :display help and quit');
      writeln('  x0=   :default: 0');
      writeln('  x1=   :default: 1');
      writeln('  y0=   :default: 0');
      writeln('  y1=   :default: 1');
      writeln('  sep=  :CSV separator; default: <space>');
      writeln('  nice  :Try to create a nicer plot by antialiasing.');      
      writeln('  qualityControl=#   :set quality control (default: 1; smaller means more samples)');
      writeln('  -<xres>x<yres> chooses resolution; default is 500x500');
      writeln('  First file name given will be interpreted as input file, the second one as output file.');
    end;

  PROCEDURE parseResolution(ps:string);
    begin
      ps:=copy(ps,2,length(ps)-1); //remove leading '-'
      xRes:=strToInt(copy(ps,1,pos('x',ps)-1));
      yRes:=strToInt(copy(ps,pos('x',ps)+1,length(ps)-1));
    end;

  VAR i:longint;
  begin
    xres:=500;
    yres:=500;
    x0:=0;
    x1:=1;
    y0:=0;
    y1:=1;
    sep:=' ';
    inputFileName:='';
    outputFileName:='';
    nicePlot:=false;
    validCmdLine:=true;
    for i:=1 to paramcount do
      if (paramstr(i)[1]='-') and (paramstr(i)[2] in ['1'..'9']) then parseResolution(paramstr(i))
      else if copy(paramstr(i),1,3)='x0='                        then x0:=strToFloat(copy(paramstr(i),4,length(paramstr(i))-3))
      else if copy(paramstr(i),1,3)='x1='                        then x1:=strToFloat(copy(paramstr(i),4,length(paramstr(i))-3))
      else if copy(paramstr(i),1,3)='y0='                        then y0:=strToFloat(copy(paramstr(i),4,length(paramstr(i))-3))
      else if copy(paramstr(i),1,3)='y1='                        then y1:=strToFloat(copy(paramstr(i),4,length(paramstr(i))-3))
      else if copy(paramstr(i),1,4)='sep='                       then sep:=(copy(paramstr(i),5,length(paramstr(i))-3))
      else if copy(paramstr(i),1,4)='nice'                       then nicePlot:=true
      else if copy(paramstr(i),1,2)='-h' then displayHelp
      else if inputFileName=''  then inputFileName:=paramstr(i)
      else if outputFileName='' then outputFileName:=paramstr(i)
      else validCmdLine:=false;
    //end for
    if validCmdLine and (inputFileName<>'') and (outputFileName<>'') then begin
      if fileExists(inputFileName)
      then generatePlot
      else begin
        writeln('Input file "',inputFileName,'" does not exist!');
        displayHelp;
      end;
    end else displayHelp;
  end;

begin
  DefaultFormatSettings.DecimalSeparator:='.';
  parseCommandline;
end.