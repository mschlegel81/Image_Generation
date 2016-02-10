UNIT workflows;
INTERFACE
USES myParams,math,mypics,myColors,sysutils;
TYPE
T_imageManipulationType=(
  {Image access:} imt_loadImage,imt_saveImage,imt_saveJpgWithSizeLimit, imt_stashImage, imt_unstashImage,
  {Geometry:}     imt_resize, imt_fit, imt_fill, imt_crop, imt_flip, imt_flop, imt_rotLeft, imt_rotRight,
  {Combination:}  imt_addRGB,   imt_subtractRGB,   imt_multiplyRGB,   imt_divideRGB,   imt_screenRGB,   imt_maxOfRGB,   imt_minOfRGB,
                  imt_addHSV,   imt_subtractHSV,   imt_multiplyHSV,   imt_divideHSV,   imt_screenHSV,   imt_maxOfHSV,   imt_minOfHSV,
                  imt_addStash, imt_subtractStash, imt_multiplyStash, imt_divideStash, imt_screenStash, imt_maxOfStash, imt_minOfStash,
                  imt_addFile,  imt_subtractFile,  imt_multiplyFile,  imt_divideFile,  imt_screenFile,  imt_maxOfFile,  imt_minOfFile,
  {per pixel color op:} imt_setColor,imt_setHue,imt_tint,imt_project,imt_limit,imt_limitLow,imt_grey,imt_sepia,
                        imt_invert,imt_abs,imt_gamma,imt_gammaRGB,imt_gammaHSV,
  {statistic color op:} imt_normalizeFull,imt_normalizeValue,imt_normalizeGrey);
                              //fk_compress,fk_compressR,fk_compressG,fk_compressB,fk_compressH,fk_compressS,fk_compressV,
                              //fk_quantize,fk_mono,
                              //fk_extract_alpha
//fk_fblur,fk_fblur_V,fk_fblur_H,fk_distFilter,fk_sharpen,fk_details,fk_lagrangeDiff,fk_nonlocalMeans,fk_rotBlur3,fk_rotBlur,fk_radBlur3,fk_radBlur,fk_cblur,fk_coarsen,fk_halftone,fk_median,fk_blurWith,fk_mode,fk_denoise

CONST
  C_imageManipulationParameterDescriptions:array[T_imageManipulationType] of T_simplifiedParameterDescription=(
  {imt_loadImage           }(name:'load';    typ:pt_fileName;         minValue:-infinity; maxValue:infinity),
  {imt_saveImage           }(name:'save';    typ:pt_fileName;         minValue:-infinity; maxValue:infinity),
  {imt_saveJpgWithSizeLimit}(name:'save';    typ:pt_jpgNameWithSize;  minValue:-infinity; maxValue:infinity),
  {imt_stashImage          }(name:'#';        typ:pt_integer; minValue:0; maxValue:infinity),
  {imt_unstashImage        }(name:'unstash#'; typ:pt_integer; minValue:0; maxValue:infinity),
  {imt_resize              }(name:'resize';  typ:pt_2integers; minValue:1; maxValue:9999),
  {imt_fit                 }(name:'fit';     typ:pt_2integers; minValue:1; maxValue:9999),
  {imt_fill                }(name:'fill';    typ:pt_2integers; minValue:1; maxValue:9999),
  {imt_crop                }(name:'crop';    typ:pt_4integers; minValue:-infinity; maxValue:infinity),
  {imt_flip                }(name:'flip'; typ:pt_none; minValue:-infinity; maxValue:infinity),
  {imt_flop                }(name:'flop'; typ:pt_none; minValue:-infinity; maxValue:infinity),
  {imt_rotLeft             }(name:'rotL'; typ:pt_none; minValue:-infinity; maxValue:infinity),
  {imt_rotRight            }(name:'rotR'; typ:pt_none; minValue:-infinity; maxValue:infinity),
  {imt_addRGB              }(name:'+RGB';      typ:pt_color; minValue:-infinity; maxValue:infinity),
  {imt_subtractRGB         }(name:'-RGB';      typ:pt_color; minValue:-infinity; maxValue:infinity),
  {imt_multiplyRGB         }(name:'*RGB';      typ:pt_color; minValue:-infinity; maxValue:infinity),
  {imt_divideRGB           }(name:'/RGB';      typ:pt_color; minValue:-infinity; maxValue:infinity),
  {imt_screenRGB           }(name:'screenRGB'; typ:pt_color; minValue:-infinity; maxValue:infinity),
  {imt_maxOfRGB            }(name:'maxRGB';    typ:pt_color; minValue:-infinity; maxValue:infinity),
  {imt_minOfRGB            }(name:'minRGB';    typ:pt_color; minValue:-infinity; maxValue:infinity),
  {imt_addHSV              }(name:'+HSV';      typ:pt_3floats; minValue:-infinity; maxValue:infinity),
  {imt_subtractHSV         }(name:'-HSV';      typ:pt_3floats; minValue:-infinity; maxValue:infinity),
  {imt_multiplyHSV         }(name:'*HSV';      typ:pt_3floats; minValue:-infinity; maxValue:infinity),
  {imt_divideHSV           }(name:'/HSV';      typ:pt_3floats; minValue:-infinity; maxValue:infinity),
  {imt_screenHSV           }(name:'screenHSV'; typ:pt_3floats; minValue:-infinity; maxValue:infinity),
  {imt_maxOfHSV            }(name:'maxHSV';    typ:pt_3floats; minValue:-infinity; maxValue:infinity),
  {imt_minOfHSV            }(name:'minHSV';    typ:pt_3floats; minValue:-infinity; maxValue:infinity),
  {imt_addStash            }(name:'+#';      typ:pt_integer; minValue:0; maxValue:infinity),
  {imt_subtractStash       }(name:'-#';      typ:pt_integer; minValue:0; maxValue:infinity),
  {imt_multiplyStash       }(name:'*#';      typ:pt_integer; minValue:0; maxValue:infinity),
  {imt_divideStash         }(name:'/#';      typ:pt_integer; minValue:0; maxValue:infinity),
  {imt_screenStash         }(name:'screen#'; typ:pt_integer; minValue:0; maxValue:infinity),
  {imt_maxOfStash          }(name:'max#';    typ:pt_integer; minValue:0; maxValue:infinity),
  {imt_minOfStash          }(name:'min#';    typ:pt_integer; minValue:0; maxValue:infinity),
  {imt_addFile             }(name:'+file:';      typ:pt_fileName; minValue:-infinity; maxValue:infinity),
  {imt_subtractFile        }(name:'-file:';      typ:pt_fileName; minValue:-infinity; maxValue:infinity),
  {imt_multiplyFile        }(name:'*file:';      typ:pt_fileName; minValue:-infinity; maxValue:infinity),
  {imt_divideFile          }(name:'/file:';      typ:pt_fileName; minValue:-infinity; maxValue:infinity),
  {imt_screenFile          }(name:'screenFile:'; typ:pt_fileName; minValue:-infinity; maxValue:infinity),
  {imt_maxOfFile           }(name:'maxFile:';    typ:pt_fileName; minValue:-infinity; maxValue:infinity),
  {imt_minOfFile           }(name:'minFile:';    typ:pt_fileName; minValue:-infinity; maxValue:infinity),
  {imt_setColor            }(name:'setRGB'; typ:pt_color; minValue:-infinity; maxValue:infinity),
  {imt_setHue              }(name:'hue';  typ:pt_float; minValue:-infinity; maxValue:infinity),
  {imt_tint                }(name:'tint'; typ:pt_float; minValue:-infinity; maxValue:infinity),
  {imt_project             }(name:'project';  typ:pt_none; minValue:-infinity; maxValue:infinity),
  {imt_limit               }(name:'limit';    typ:pt_none; minValue:-infinity; maxValue:infinity),
  {imt_limitLow            }(name:'limitLow'; typ:pt_none; minValue:-infinity; maxValue:infinity),
  {imt_grey                }(name:'grey';     typ:pt_none; minValue:-infinity; maxValue:infinity),
  {imt_sepia               }(name:'sepia';    typ:pt_none; minValue:-infinity; maxValue:infinity),
  {imt_invert              }(name:'invert';   typ:pt_none; minValue:-infinity; maxValue:infinity),
  {imt_abs                 }(name:'abs';      typ:pt_none; minValue:-infinity; maxValue:infinity),
  {imt_gamma               }(name:'gamma';    typ:pt_float;   minValue:1E-3; maxValue:infinity),
  {imt_gammaRGB            }(name:'gammaRGB'; typ:pt_3floats; minValue:1E-3; maxValue:infinity),
  {imt_gammaHSV            }(name:'gammaHSV'; typ:pt_3floats; minValue:1E-3; maxValue:infinity),
  {imt_normalizeFull       }(name:'normalize';  typ:pt_none; minValue:-infinity; maxValue:infinity),
  {imt_normalizeValue      }(name:'normalizeV'; typ:pt_none; minValue:-infinity; maxValue:infinity),
  {imt_normalizeGrey       }(name:'normalizeG'; typ:pt_none; minValue:-infinity; maxValue:infinity)
   );
TYPE
  P_imageManipulationWorkflow=^T_imageManipulationWorkflow;

  T_imageManipulationStep=object
    imageManipulationType:T_imageManipulationType;
    param:T_parameterValue;

    CONSTRUCTOR create(CONST t:T_imageManipulationType; CONST p:T_parameterValue);
    DESTRUCTOR destroy;

    PROCEDURE execute(CONST image:P_rawImage; CONST workflow:P_imageManipulationWorkflow);
  end;

  T_imageManipulationWorkflow=object
    private
      imageStash:array of P_rawImage;
      step:array of record
             manipulation:T_imageManipulationStep;
             intermediate:P_rawImage;
             stepMessage :ansistring;
           end;
      progress:record
        stepNumber:longint;
        stepMessage:ansistring;
        done:boolean;
      end;
      hasError:boolean;

    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;

      PROCEDURE raiseError(CONST message:ansistring);
      PROCEDURE setCurrentStepMessage(CONST message:ansistring);

      PROCEDURE clearStash;
      PROCEDURE clearIntermediate;

      PROCEDURE execute;
      PROCEDURE executeOnImage(CONST img:P_rawImage; CONST storeIntermediate:boolean; CONST skipFirst:boolean=true);

      FUNCTION executing:boolean;
      FUNCTION executingStep:longint;
      FUNCTION executingFunction:ansistring;

  end;


IMPLEMENTATION
CONSTRUCTOR T_imageManipulationStep.create(CONST t:T_imageManipulationType; CONST p:T_parameterValue);
  begin
    imageManipulationType:=t;
    param:=p;
  end;

DESTRUCTOR T_imageManipulationStep.destroy;
  begin

  end;

PROCEDURE T_imageManipulationStep.execute(CONST image: P_rawImage; CONST workflow:P_imageManipulationWorkflow);
  PROCEDURE stash;
    VAR oldLength,i:longint;
    begin
      with workflow^ do begin
        if param.i0>length(imageStash) then begin
          oldLength:=length(imageStash);
          setLength(imageStash,param.i0+1);
          for i:=oldLength to length(imageStash)-1 do imageStash[i]:=nil;
        end;
        if imageStash[param.i0]<>nil then dispose(imageStash[param.i0],destroy);
        new(imageStash[param.i0],create(image^));
      end;
    end;

  PROCEDURE unstash;
    begin
      with workflow^ do if (param.i0<0) or (param.i0>length(imageStash)) then raiseError('Invalid stash Index')
      else image^.copyFromImage(imageStash[param.i0]^);
    end;

  FUNCTION plausibleResolution:boolean;
    begin
      if (param.i0>0) and (param.i0<10000) and (param.i1>0) and (param.i1<10000) then result:=true
      else begin
        result:=false;
        workflow^.raiseError('Invalid resolution; Both values must be in range 1..9999.');
      end;
    end;

  FUNCTION colMult  (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do result[i]:=a[i]*b[i]; end;
  PROCEDURE combine;
    FUNCTION colDiv   (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do result[i]:=a[i]/b[i]; end;
    FUNCTION colScreen(CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do result[i]:=1-(1-a[i])*(1-b[i]); end;
    FUNCTION colMax   (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do if a[i]>b[i] then result[i]:=a[i] else result[i]:=b[i]; end;
    FUNCTION colMin   (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do if a[i]<b[i] then result[i]:=a[i] else result[i]:=b[i]; end;
    VAR x,y:longint;
        other:P_rawImage;
        c1:T_floatColor;

    begin
      case imageManipulationType of
        imt_addStash..imt_minOfStash:
          with workflow^ do if (param.i0>=0) and (param.i0<length(imageStash))
          then raiseError('Invalid stash Index')
          else other:=imageStash[param.i0];
        imt_addFile..imt_minOfFile : new(other,create(param.fileName));
        imt_addRGB..imt_minOfRGB: begin
          c1:=param.color;
          case imageManipulationType of
            imt_addRGB      : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=          image^[x,y]+c1;
            imt_subtractRGB : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=          image^[x,y]-c1;
            imt_multiplyRGB : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colMult  (image^[x,y],c1);
            imt_divideHSV   : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colDiv   (image^[x,y],c1);
            imt_screenHSV   : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colScreen(image^[x,y],c1);
            imt_maxOfHSV    : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colMax   (image^[x,y],c1);
            imt_minOfHSV    : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colMin   (image^[x,y],c1);
          end;
          exit;
        end;
        imt_addHSV..imt_minOfHSV: begin
          c1:=param.color;
          case imageManipulationType of
            imt_addHSV      : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(          toHSV(image^[x,y])+c1);
            imt_subtractHSV : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(          toHSV(image^[x,y])-c1);
            imt_multiplyHSV : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(colMult  (toHSV(image^[x,y]),c1));
            imt_divideHSV   : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(colDiv   (toHSV(image^[x,y]),c1));
            imt_screenHSV   : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(colScreen(toHSV(image^[x,y]),c1));
            imt_maxOfHSV    : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(colMax   (toHSV(image^[x,y]),c1));
            imt_minOfHSV    : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(colMin   (toHSV(image^[x,y]),c1));
          end;
          exit;
        end;

      end;
      case imageManipulationType of
        imt_addStash     ,imt_addFile      : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=image^[x,y]+other^[x,y];
        imt_subtractStash,imt_subtractFile : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=image^[x,y]-other^[x,y];
        imt_multiplyStash,imt_multiplyFile : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colMult  (image^[x,y],other^[x,y]);
        imt_divideStash  ,imt_divideFile   : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colDiv   (image^[x,y],other^[x,y]);
        imt_screenStash  ,imt_screenFile   : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colScreen(image^[x,y],other^[x,y]);
        imt_maxOfStash   ,imt_maxOfFile    : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colMax   (image^[x,y],other^[x,y]);
        imt_minOfStash   ,imt_minOfFile    : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colMin   (image^[x,y],other^[x,y]);
      end;
      if imageManipulationType in [imt_addFile..imt_minOfFile] then dispose(other,destroy);
    end;

  PROCEDURE colorOp;
    FUNCTION limitHigh(CONST x:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do if x[i]>1 then result[i]:=1 else result[i]:=x[i]; end;
    FUNCTION limitLow(CONST x:T_floatColor):T_floatColor;  inline; VAR i:byte; begin for i:=0 to 2 do if x[i]<0 then result[i]:=0 else result[i]:=x[i]; end;
    VAR x,y:longint;
    begin
      case imageManipulationType of
        imt_setColor: for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=param.color;
        imt_setHue:   for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=hue(image^[x,y],param.f0);
        imt_tint:     for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=tint(image^[x,y],param.f0);
        imt_project:  for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=projectedColor(image^[x,y]);
        imt_limit:    for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=limitHigh(limitLow(image^[x,y]));
        imt_limitLow: for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=          limitLow(image^[x,y]);
        imt_grey    : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=    subjectiveGrey(image^[x,y]);
        imt_sepia   : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=             sepia(image^[x,y]);
        imt_invert:   for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=            invert(image^[x,y]);
        imt_abs:      for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=            absCol(image^[x,y]);
        imt_gamma:    for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=             gamma(image^[x,y],param.f0,param.f1,param.f2);
        imt_gammaRGB: for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=             gamma(image^[x,y],param.f0,param.f1,param.f2);
        imt_gammaHSV: for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=          gammaHSV(image^[x,y],param.f0,param.f1,param.f2);
      end;
    end;

  PROCEDURE statisticColorOp;
    VAR compoundHistogram:T_compoundHistogram;
        greyHist:T_histogram;
        p0,p1:T_floatColor;
        x,y:longint;
    FUNCTION normValue(CONST c:T_floatColor):T_floatColor;
      begin
        result:=toHSV(c);
        result[2]:=(result[2]-p0[0])*p1[0];
        result:=fromHSV(result);
      end;

    begin
      case imageManipulationType of
        imt_normalizeFull: begin
          compoundHistogram:=image^.histogram;
          p0[0]:=compoundHistogram.R.percentile(0.1);
          p0[1]:=compoundHistogram.G.percentile(0.1);
          p0[2]:=compoundHistogram.B.percentile(0.1);
          p1[0]:=1/(compoundHistogram.R.percentile(99.9)-p0[0]);
          p1[1]:=1/(compoundHistogram.G.percentile(99.9)-p0[1]);
          p1[2]:=1/(compoundHistogram.B.percentile(99.9)-p0[2]);
          for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colMult(image^[x,y]-p0,p1);
          if compoundHistogram.mightHaveOutOfBoundsValues then statisticColorOp;
          compoundHistogram.destroy;
        end;
        imt_normalizeValue: begin
          compoundHistogram:=image^.histogramHSV;
          p0[0]:=compoundHistogram.B.percentile(0.1);
          p1[0]:=1/(compoundHistogram.B.percentile(99.9)-p0[0]);
          for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=normValue(image^[x,y]);
          if compoundHistogram.B.mightHaveOutOfBoundsValues then statisticColorOp;
          compoundHistogram.destroy;
        end;
        imt_normalizeGrey: begin
          compoundHistogram:=image^.histogram;
          greyHist:=compoundHistogram.subjectiveGreyHistogram;
          p0:=greyHist.percentile(0.1)*white;
          p1[0]:=1/(greyHist.percentile(99.9)-p0[0]);
          for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=(image^[x,y]-p0)*p1[0];
          if greyHist.mightHaveOutOfBoundsValues then statisticColorOp;
          greyHist.destroy;
          compoundHistogram.destroy;
        end;
      end;
    end;

  begin
    case imageManipulationType of
      imt_loadImage: image^.loadFromFile(param.fileName);
      imt_saveImage: image^.saveToFile(param.fileName);
      imt_saveJpgWithSizeLimit: image^.saveJpgWithSizeLimitReturningErrorOrBlank(param.fileName,param.i0);
      imt_stashImage: stash;
      imt_unstashImage: unstash;
      imt_resize: if plausibleResolution then image^.resize(param.i0,param.i1,res_exact);
      imt_fit   : if plausibleResolution then image^.resize(param.i0,param.i1,res_fit);
      imt_fill  : if plausibleResolution then image^.resize(param.i0,param.i1,res_cropToFill);
      imt_crop  : image^.crop(param.i0,param.i1,param.i2,param.i3);
      imt_flip  : image^.flip;
      imt_flop  : image^.flop;
      imt_rotLeft : image^.rotLeft;
      imt_rotRight: image^.rotRight;
      imt_addRGB..imt_minOfFile: combine;
      imt_setColor, imt_setHue, imt_tint, imt_project, imt_limit,imt_limitLow,imt_grey,imt_sepia,imt_invert,imt_abs,imt_gamma,imt_gammaRGB,imt_gammaHSV: colorOp;
      imt_normalizeFull,imt_normalizeValue,imt_normalizeGrey:statisticColorOp;
    end;
  end;


CONSTRUCTOR T_imageManipulationWorkflow.create;
  begin

  end;

DESTRUCTOR T_imageManipulationWorkflow.destroy;
  VAR i:longint;
  begin
    clearIntermediate;
    clearStash;
    for i:=0 to length(step)-1 do step[i].manipulation.destroy;
    setLength(step,0);
  end;

PROCEDURE T_imageManipulationWorkflow.raiseError(CONST message: ansistring);
  begin
    hasError:=true;
    if displayErrorFunction<>nil
    then displayErrorFunction(message)
    else writeln(stderr,message);
    beep;
  end;

PROCEDURE T_imageManipulationWorkflow.setCurrentStepMessage(
  CONST message: ansistring);
  begin
    if (progress.stepNumber>=0) and (progress.stepNumber<length(step)) then begin
      step[progress.stepNumber].stepMessage:=message;
      writeln(stderr,message);
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.clearIntermediate;
  VAR i:longint;
  begin
    for i:=0 to length(step)-1 do with step[i] do begin
      if intermediate<>nil then dispose(intermediate,destroy);
      intermediate:=nil;
      stepMessage:='';
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.clearStash;
  VAR i:longint;
  begin
    for i:=0 to length(imageStash)-1 do if imageStash[i]<>nil then dispose(imageStash[i],destroy);
    setLength(imageStash,0);
  end;

PROCEDURE T_imageManipulationWorkflow.execute;
  VAR img:T_rawImage;
  begin
    if length(step)=0 then exit;
    progress.stepNumber :=0;
    progress.done:=false;
    if (step[0].manipulation.imageManipulationType=imt_loadImage) then begin
      img.create(step[0].manipulation.param.fileName);
    end else if (step[0].manipulation.imageManipulationType=imt_resize) then begin
      img.create(step[0].manipulation.param.i0,
                 step[0].manipulation.param.i1);
    end else begin
      raiseError('Workflow must begin with loading an image or defining an image with a given resolution!');
      exit;
    end;
    executeOnImage(@img,false,true);
    img.destroy;
  end;

PROCEDURE T_imageManipulationWorkflow.executeOnImage(CONST img: P_rawImage;
  CONST storeIntermediate: boolean; CONST skipFirst: boolean);
  PROCEDURE stepDone(CONST storeRecommended:boolean);
    begin
      if storeIntermediate and storeRecommended then with step[progress.stepNumber] do begin
        new(intermediate,create(img^));
        intermediate^.mutateType(rs_24bit);
      end;
      inc(progress.stepNumber);
      if progress.stepNumber<length(step) then step[progress.stepNumber].stepMessage:='';
    end;

  begin
    clearIntermediate;
    clearStash;
    progress.stepNumber :=0;
    progress.stepMessage:='';
    if skipFirst then begin
      setCurrentStepMessage('Initial image');
      stepDone(true);
    end;
    while not(hasError) and (progress.stepNumber<length(step)) do with step[progress.stepNumber] do begin
      manipulation.execute(img,@self);
      stepDone(not(manipulation.imageManipulationType in [imt_saveImage, imt_saveJpgWithSizeLimit, imt_stashImage]));
    end;
    progress.done:=true;
    clearStash;
  end;

FUNCTION T_imageManipulationWorkflow.executing: boolean;
  begin
    result:=not(progress.done or hasError);
  end;

FUNCTION T_imageManipulationWorkflow.executingStep: longint;
  begin result:=progress.stepNumber; end;

FUNCTION T_imageManipulationWorkflow.executingFunction: ansistring;
  begin result:=progress.stepMessage; end;

end.
