UNIT ig_fractals;
INTERFACE
USES imageGeneration,mypics,myColors,complex,myParams,math,mySys,sysutils,myTools;
CONST LIGHT_NORMAL_INDEX=10;
TYPE
  P_functionPerPixelViaRawDataAlgorithm=^T_functionPerPixelViaRawDataAlgorithm;
  T_functionPerPixelViaRawDataAlgorithm=object(T_functionPerPixelAlgorithm)
    temporaryRawMap:P_rawImage;
    rawMapIsOutdated:boolean;

    maxDepth    :longint;
    colorSource :byte;
    colorStyle  :byte;
    colorVariant:byte;
    pseudoGamma :double;
    lightNormal :T_floatColor;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    PROCEDURE iterationStart(VAR c:T_Complex; OUT x:T_Complex); virtual; abstract;
    PROCEDURE iterationStep(CONST c:T_Complex; VAR x:T_Complex); virtual; abstract;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor;
    FUNCTION getColor(CONST rawData:T_floatColor):T_floatColor; virtual;
    FUNCTION getColorAt(CONST ix,iy:longint; CONST xy:T_Complex):T_floatColor; virtual;
    PROCEDURE prepareRawMap(CONST yStart:longint); virtual;
    PROCEDURE prepareImage(CONST forPreview:boolean=false); virtual;
  end;

  P_rawDataWorkerThreadTodo=^T_rawDataWorkerThreadTodo;

  { T_rawDataWorkerThreadTodo }

  T_rawDataWorkerThreadTodo=object(T_queueToDo)
    algorithm:P_functionPerPixelViaRawDataAlgorithm;
    y:longint;

    CONSTRUCTOR create(CONST algorithm_:P_functionPerPixelViaRawDataAlgorithm; CONST y_:longint);
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual;
  end;


  T_newton3Algorithm=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    PROCEDURE iterationStart(VAR c:T_Complex; OUT x:T_Complex); virtual;
    PROCEDURE iterationStep(CONST c:T_Complex; VAR x:T_Complex); virtual;
  end;

IMPLEMENTATION

{ T_rawDataWorkerThreadTodo }

CONSTRUCTOR T_rawDataWorkerThreadTodo.create(CONST algorithm_: P_functionPerPixelViaRawDataAlgorithm; CONST y_: longint);
  begin
    inherited create;
    algorithm:=algorithm_;
    y:=y_;
  end;

DESTRUCTOR T_rawDataWorkerThreadTodo.destroy;
  begin
  end;

PROCEDURE T_rawDataWorkerThreadTodo.execute;
  begin
    algorithm^.prepareRawMap(y);
    progressor.logStepDone;
  end;

CONSTRUCTOR T_functionPerPixelViaRawDataAlgorithm.create;
  begin
    inherited create;
    temporaryRawMap:=nil;
    rawMapIsOutdated:=false;
    addParameter('depth',pt_integer,0);
    addParameter('source',pt_enum,0,9,
      'final',
      'average',
      'floating_average',
      'path_chaos',
      'final_angle',
      'steps_to_divergence',
      'avg_chaos',
      'avg_sqr_chaos',
      'max_chaos',
      'normal');
    addParameter('style',pt_enum,0,9,
      'fire/metal',
      'water/glass',
      'spectrum/plastic',
      'trafficLight/fire',
      'earth/drugged',
      'greyscale/gold',
      'zebra/levels',
      'greenzebra/strange',
      'rainbow/window',
      'discrete/line');
    addParameter('variant',pt_enum,0,3,
      'direct',
      'inverted',
      'parabola',
      'inv_parabola');
    addParameter('gamma',pt_float,1E-3,1E3);
    addParameter('light_normal',pt_3floats,-1,1);
    resetParameters(0);
  end;

DESTRUCTOR T_functionPerPixelViaRawDataAlgorithm.destroy;
  begin
    if temporaryRawMap<>nil then dispose(temporaryRawMap,destroy);
    temporaryRawMap:=nil;
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    maxDepth    :=100;
    colorSource :=0;
    colorStyle  :=0;
    colorVariant:=0;
    pseudoGamma :=1;
    lightNormal :=newColor(0,0,1);
    rawMapIsOutdated:=true;
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+6;
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  FUNCTION normedVector(CONST x:T_floatColor):T_floatColor;
    VAR len:double;
    begin
      len:=sqrt(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]);
      if len<1E-10 then result:=newColor(0,0,1)
      else result:=x*(1/len);
    end;

  begin
    if index<inherited numberOfParameters then begin
      inherited setParameter(index,value);
      if index<SCALER_PARAMETER_COUNT then rawMapIsOutdated:=true;
    end else case byte(index-inherited numberOfParameters) of
      0: begin rawMapIsOutdated:=rawMapIsOutdated or (maxDepth<>value.i0); maxDepth:=value.i0; end;
      1: begin rawMapIsOutdated:=rawMapIsOutdated or (colorSource div 3 <> value.i0 div 3); colorSource:=value.i0; end;
      2: colorStyle:=value.i0;
      3: colorVariant:=value.i0;
      4: pseudoGamma:=value.f0;
      5: lightNormal:=normedVector(value.color);
    end;
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index<inherited numberOfParameters
    then result:=inherited getParameter(index)
    else case byte(index-inherited numberOfParameters) of
      0: result.createFromValue(parameterDescription(inherited numberOfParameters  ),maxDepth);
      1: result.createFromValue(parameterDescription(inherited numberOfParameters+1),colorSource);
      2: result.createFromValue(parameterDescription(inherited numberOfParameters+2),colorStyle);
      3: result.createFromValue(parameterDescription(inherited numberOfParameters+3),colorVariant);
      4: result.createFromValue(parameterDescription(inherited numberOfParameters+4),pseudoGamma);
      5: result.createFromValue(parameterDescription(inherited numberOfParameters+5),lightNormal);
    end;
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  CONST h0:T_Complex=(re: 0.149429245361342; im: 0.557677535825206);
        h1:T_Complex=(re: 0.408248290463863; im:-0.408248290463863);
        h2:T_Complex=(re:-0.557677535825206; im:-0.149429245361342);

  FUNCTION toSphere(CONST x:T_Complex):T_floatColor; inline;
    VAR t:double;
    begin
      t:=4/(4+x.re*x.re+x.im*x.im);
      result[0]:=x.re*t;
      result[1]:=x.im*t;
      result[2]:=t*2;
    end;

  FUNCTION toSphereZ(CONST x:T_Complex):double; inline;
    begin
      result:=4/(4+x.re*x.re+x.im*x.im);
      result:=result*2;
    end;

  FUNCTION dist  (CONST x,y:T_floatColor):double; inline; begin result:=sqrt(system.sqr(x[0]-y[0])+system.sqr(x[1]-y[1])+system.sqr(x[2]-y[2])); end;
  FUNCTION sqDist(CONST x,y:T_floatColor):double; inline; begin result:=     system.sqr(x[0]-y[0])+system.sqr(x[1]-y[1])+system.sqr(x[2]-y[2]) ; end;

  VAR i:longint;
      x,x1,x2,
      c,c1,c2:T_Complex;
      r,s,v:T_floatColor;
      d0,d1,d2:double;
      sphereZ:double;
  begin
    c:=xy;
    i:=0;
    result:=black;
    s:=black;
    v:=black;
    case colorSource of
      0..2: begin
        iterationStart(c,x);
        while (i<maxDepth) and (isValid(x)) do begin
          iterationStep(c,x);
          sphereZ:=toSphereZ(x);
          result[1]:=result[1]+sphereZ;
          result[2]:=result[2]*0.95+0.05*sphereZ;
          inc(i);
        end;
        sphereZ:=toSphereZ(x);
        result[0]:=0.5*sphereZ;
        result[1]:=result[1]+(maxDepth-i)*sphereZ;
        result[1]:=0.5*(result[1]*(1/maxDepth));
        sphereZ:=sphereZ*0.05;
        while (i<maxDepth) do begin result[2]:=result[2]*0.95+sphereZ; inc(i); end;
        result[2]:=0.5*result[2];
      end;
      3..5: begin
        iterationStart(c,x);
        while (i<maxDepth) and (x.re*x.re+x.im*x.im<1E6) do begin
          iterationStep(c,x);
          r:=toSphere(x);
          s:=s+r;
          v:=v+newColor(r[0]*r[0],r[1]*r[1],r[2]*r[2]);
          inc(i);
        end;
        result[2]:=i/maxDepth;
        while (i<maxDepth) and (x.re*x.re+x.im*x.im<1E10) do begin
          iterationStep(c,x);
          r:=toSphere(x);
          s:=s+r;
          v:=v+newColor(r[0]*r[0],r[1]*r[1],r[2]*r[2]);
          inc(i);
        end;
        s:=s+(maxDepth-i)*r;
        v:=v+(maxDepth-i)*newColor(r[0]*r[0],r[1]*r[1],r[2]*r[2]);
        result[0]:=((v[0]+v[1]+v[2])/maxDepth-(s*s)*(1/(maxDepth*maxDepth)));
        result[1]:=arg(x)/(2*pi); if result[1]<0 then result[1]:=result[1]+1;
      end;
      //to do: chaos measure scaled by diagonal, not by pixels!!!
      6..8 : begin
        c :=xy+1/(scaler.getZoom*1414)*h0; iterationStart(c ,x );
        c1:=xy+1/(scaler.getZoom*1414)*h1; iterationStart(c1,x1);
        c2:=xy+1/(scaler.getZoom*1414)*h2; iterationStart(c2,x2);
        iterationStart(c,x);
        r:=black;
        s:=black;
        v:=black;
        while isValid(x) and isValid(x1) and isValid(x2) and (i<maxDepth) do begin
          iterationStep(c ,x ); r:=r*0.9+0.1*toSphere(x );
          iterationStep(c1,x1); s:=s*0.9+0.1*toSphere(x1);
          iterationStep(c2,x2); v:=v*0.9+0.1*toSphere(x2);
          inc(i);
        end;
        while (i<maxDepth) do begin
          r:=r*0.9+0.1*toSphere(x );
          s:=s*0.9+0.1*toSphere(x1);
          v:=v*0.9+0.1*toSphere(x2);
          inc(i);
        end;
        result[0]:=(dist(r,s)+dist(s,v)+dist(r,v))*0.166666666666667 ;
        result[1]:=sqrt(sqDist(r,s)+sqDist(s,v)+sqDist(r,s))*0.288675134594813;
        result[2]:=max(dist(r,s),max(dist(s,v),dist(r,v)))*0.5;
      end;
      else begin
        c :=xy+h0*scaler.getAbsoluteZoom; iterationStart(c ,x );
        c1:=xy+h1*scaler.getAbsoluteZoom; iterationStart(c1,x1);
        c2:=xy+h2*scaler.getAbsoluteZoom; iterationStart(c2,x2);

        d0:=0; d1:=0;  d2:=0; i:=0;
        while isValid(x) and isValid(x1) and isValid(x2) and (i<maxDepth) do begin
          iterationStep(c ,x ); d0:=d0+toSphereZ(x);
          iterationStep(c1,x1); d1:=d1+toSphereZ(x1);
          iterationStep(c2,x2); d2:=d2+toSphereZ(x2);
          inc(i);
        end;
        //compute and normalize normal vector:-----------------//
        d0:=sqrt(d0/maxDepth);                                 //
        d1:=sqrt(d1/maxDepth)-d0;                              //
        d2:=sqrt(d2/maxDepth)-d0;                              //
        d0:=1/sqrt(d1*d1+d2*d2+scaler.getAbsoluteZoom*scaler.getAbsoluteZoom);
        result[2]:=(scaler.getAbsoluteZoom*d0);                //
        result[0]:=(d1                    *d0);                //
        result[1]:=(d2                    *d0);                //
        //-------------------:compute and normalize normal vector
      end;
    end;
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.getColor(CONST rawData: T_floatColor): T_floatColor;
  FUNCTION fire(x:double):T_floatColor; inline;
    begin
      if x<0 then result:=newColor(0,0,0)
      else if x<1/3 then result:=newColor(3*x,0,0)
      else if x<2/3 then result:=newColor(1,3*x-1,0)
      else if x<1   then result:=newColor(1,1,3*x-2)
      else result:=newColor(1,1,1);
    end;

  FUNCTION water(x:double):T_floatColor; inline;
    begin
      if x<0 then result:=newColor(0,0,0)
      else if x<1/2 then result:=newColor(0,0,2*x)
      else if x<1   then result:=newColor(2*x-1,2*x-1,1)
      else result:=newColor(1,1,1);
    end;

  FUNCTION spectrum(x:double):T_floatColor; inline;
    begin
      x:=x*6;
      if      x<0   then result:=newColor(0,0,0)
      else if x<0.5 then result:=newColor(2*x,0,0)
      else if x<1.5 then result:=newColor(1,x-0.5,0)
      else if x<2.5 then result:=newColor(1-(x-1.5),1,0)
      else if x<3.5 then result:=newColor(0,1,x-2.5)
      else if x<4.5 then result:=newColor(0,1-(x-3.5),1)
      else if x<5.5 then result:=newColor(x-4.5,0,1)
      else if x<6   then result:=newColor(1-2*(x-5.5),0,1-2*(x-5.5))
      else               result:=newColor(0,0,0);
    end;

  FUNCTION trafficLight(x:double):T_floatColor; inline;
    begin
      if x<0 then result:=newColor(0.8,0,0)
      else if x<1/2 then result:=newColor(1,2*x,0)
      else if x<1   then result:=newColor(1-(2*x-1),1,0)
      else result:=newColor(0,0.8,0);
    end;

  FUNCTION earth(x:double):T_floatColor; inline;
    begin
      if x<0 then result:=newColor(0,0,0)
      else if x<1/3 then result:=newColor(3*x,1.5*x,0)
      else if x<2/3 then result:=newColor(  1-0.3*(3*x-1),0.5+0.2*(3*x-1),0.7*(3*x-1))
      else if x<1   then result:=newColor(0.7+0.3*(3*x-2),0.7+0.3*(3*x-2),0.7+0.3*(3*x-2))
      else result:=newColor(1,1,1);
    end;

  FUNCTION greyscale(x:double):T_floatColor; inline;
    begin
      if x<0 then result:=newColor(0,0,0)
      else if x<1   then result:=newColor(x,x,x)
      else result:=newColor(1,1,1);
    end;

  FUNCTION zebra(x:double):T_floatColor; inline;
    VAR q:longint;
    begin
      if x<0 then x:=0 else if x>1 then x:=1;
      q:=round(128*x);
      if odd(q) then result:=newColor(0,0,0)
                else result:=newColor(1,1,1);
    end;

  FUNCTION greenzebra(x:double):T_floatColor; inline;
    VAR q:longint;
    begin
      if x<0 then x:=0 else if x>1 then x:=1;
      q:=round(128*x);
      if odd(q) then result:=newColor(0,x,0)
                else result:=newColor(1-x,1-x,1-x);
    end;

  FUNCTION rainbow(x:double):T_floatColor; inline;
    begin
      if x<0 then x:=0
      else if x>1 then x:=6
      else x:=6*x;
      if      x<1 then result:=newColor(1  ,x  ,0  )
      else if x<2 then result:=newColor(2-x,1  ,0  )
      else if x<3 then result:=newColor(0  ,1  ,x-2)
      else if x<4 then result:=newColor(0  ,4-x,1  )
      else if x<5 then result:=newColor(x-4,0  ,1  )
      else             result:=newColor(1  ,0  ,6-x);
    end;

  FUNCTION discrete(x:double):T_floatColor; inline;
    VAR i:byte;
    begin
      if      x<0 then i:=0
      else if x>1 then i:=15
                  else i:=byte(floor(x*16));
      case i of
        0: result:=newColor(0,0,0);
        1: result:=newColor(0.5,0,0);
        2: result:=newColor(1,0,0);
        3: result:=newColor(1,0.25,0);
        4: result:=newColor(1,0.5,0);
        5: result:=newColor(0.9,0.65,0);
        6: result:=newColor(0.8,0.8,0);
        7: result:=newColor(0.4,0.9,0);
        8: result:=newColor(0,1,0);
        9: result:=newColor(0,0.9,0.4);
       10: result:=newColor(0,0.8,0.8);
       11: result:=newColor(0,0.4,0.9);
       12: result:=newColor(0,0,1);
       13: result:=newColor(0.5,0,1);
       14: result:=newColor(1,0,1);
      else result:=newColor(1,1,1);
      end;
    end;

  FUNCTION normalToColor_metal(CONST n:T_floatColor):T_floatColor;
    VAR diffuse,spec4,spec8,spec16:single;
    CONST colAmb   :T_floatColor=(0.1,0.1,0.1);
          colDiff  :T_floatColor=(0.3,0.3,0.3);
          colSpec05:T_floatColor=(0.6,0.0,0.0);
          colSpec08:T_floatColor=(0.0,0.6,0.0);
          colSpec16:T_floatColor=(0.0,0.0,0.6);

    begin
      diffuse :=lightNormal*n;
      spec4:=-(lightNormal[2]-2*n[2]*diffuse);
      if spec4<0 then begin
      result:=colAmb+colDiff*diffuse;
      end else begin
        spec4 :=spec4*spec4;
        spec4 :=spec4*spec4;
        spec8 :=spec4*spec4;
        spec16:=spec8*spec8;
        result:=colAmb+colDiff*diffuse
                      +colSpec05*spec4
                      +colSpec08*spec8
                      +colSpec16*spec16;
      end;

    end;

  FUNCTION normalToColor_glass(CONST n:T_floatColor):T_floatColor;
    VAR diffuse,spec:single;
    CONST colAmb   :T_floatColor=(0.0,0.0,0.2);
          colDiff  :T_floatColor=(0.0,0.0,0.5);
          colSpec  :T_floatColor=(0.8,0.8,0.8);
    begin
      diffuse :=lightNormal*n;
      spec:=-(lightNormal[2]-2*n[2]*diffuse);
      if spec<0.999 then begin
      result:=colAmb+colDiff*diffuse;
      end else begin
        result:=colAmb+colDiff*diffuse
                      +colSpec*(spec-0.999)*1000;
      end;
    end;

  FUNCTION normalToColor_drugged(CONST n:T_floatColor):T_floatColor;
    CONST colAmb   :T_floatColor=(0.3,0.3,0.3);
          colDiff  :T_floatColor=(0.3,0.3,0.3);
    VAR spec,diffuse:single;
    begin
      diffuse :=lightNormal*n;
      spec:=-(lightNormal[2]-2*n[2]*diffuse);
      if spec<1 then
      result:=colAmb+colDiff*diffuse+spec*hue(spec)
      else result:=black;
    end;

  FUNCTION normalToColor_plastic(CONST n:T_floatColor):T_floatColor;
    CONST colAmb   :T_floatColor=(0.5,0.0,0.0);
          colDiff  :T_floatColor=(0.5,0.0,0.0);
          colSpec  :T_floatColor=(0.0,1,1);
    VAR spec,diffuse:single;
    begin
      diffuse :=lightNormal*n;
      spec:=-(lightNormal[2]-2*n[2]*diffuse);
      if spec<0 then result:=colAmb+colDiff*diffuse else begin
        spec:=spec*spec;
        spec:=spec*spec;
        spec:=spec*spec;
        result:=colAmb+colDiff*diffuse+colSpec*spec;
      end;
    end;

  FUNCTION normalToColor_fire(CONST n:T_floatColor):T_floatColor;
    VAR x:single;
    begin
      x:=(lightNormal*n);
      x:=1.5+1.5*x;//(lightNormal[2]-2*n[2]*x);
      if      x>2 then begin result[0]:=1; result[1]:=1;   result[2]:=x-2; end
      else if x>1 then begin result[0]:=1; result[1]:=x-1; result[2]:=0;   end
      else             begin result[0]:=x; result[1]:=0;   result[2]:=0;   end;
    end;

  FUNCTION normalToColor_gold(CONST n:T_floatColor):T_floatColor;
    CONST colAmb   :T_floatColor=(0.5,0.25, 0);
          colDiff  :T_floatColor=(0.5,0.5,0);
          colSpec  :T_floatColor=(0.5,0.25,0.5);
    VAR spec,diffuse:single;
    begin
      diffuse :=lightNormal*n;
      spec:=-(lightNormal[2]-2*n[2]*diffuse);
      if (spec<0) then result:=colAmb+colDiff*diffuse else begin
        spec:=spec*spec;
        spec:=spec*spec;
        spec:=spec*spec;
        result:=projectedColor(colAmb+colDiff*diffuse+colSpec*spec);
      end;
    end;

  FUNCTION normalToColor_levels(CONST n:T_floatColor):T_floatColor;
    begin
      result:=white*(round(5+5*(lightNormal*n))*0.1);
    end;

  FUNCTION normalToColor_window(n:T_floatColor):T_floatColor;
    CONST colAmbient:T_floatColor=(0.1,0.05,0.0);
          colDiffuse:T_floatColor=(0.4,0.20,0.0);

    VAR diffuse:single;
        specVec:T_floatColor;
    begin
      diffuse:=lightNormal*n;
      specVec:=lightNormal-n*(2*diffuse);
      result:=colAmbient+diffuse*colDiffuse;
      if (specVec[2]<0) then begin
        if (abs(specVec[1])<0.2 ) and (abs(specVec[0])<0.2 ) and
           (abs(specVec[1])>0.02) and (abs(specVec[0])>0.02) then result:=result+white*0.75
        else result:=result+white*(0.5*specVec[2]*specVec[2]);
      end;
    end;

  FUNCTION normalToColor_line(CONST n:T_floatColor):T_floatColor;
    CONST colAmbient:T_floatColor=(0.0,0.3,0.0);
          colDiffuse:T_floatColor=(0.0,0.3,0.0);

    VAR diffuse:single;
        specVec:T_floatColor;
    begin
      diffuse:=lightNormal*n;
      specVec:=lightNormal-n*(2*diffuse);
      result:=colAmbient+diffuse*colDiffuse;
      if (specVec[2]<0) then begin
        specVec[1]:=specVec[1]+specVec[0];
        if (abs(specVec[1])<0.02) then result:=result-white*specVec[2]
        else result:=result+white*(0.7*specVec[2]*specVec[2]*(1-specVec[1]));
      end;
    end;

  FUNCTION normalToColor_strange(CONST n:T_floatColor):T_floatColor;
    CONST colAmbient:T_floatColor=(0.1,0.1,0.1);
          colDiffuse:T_floatColor=(0.4,0.4,0.4);

          vec1:T_floatColor=( 0                  , 0.18257418276333211,0.98319208082057976);
          vec2:T_floatColor=( 0.15811387502784748,-0.09128710059683582,0.98319208082057976);
          vec3:T_floatColor=(-0.15811389098898917,-0.0912870729513256 ,0.98319208082057976);
    VAR diffuse:single;
        specVec,spec:T_floatColor;

    begin
      diffuse:=lightNormal*n;
      specVec:=lightNormal-n*(2*diffuse);
      spec[0]:=specVec*vec1; if spec[0]>0 then spec[0]:=0;
      spec[1]:=specVec*vec2; if spec[1]>0 then spec[1]:=0;
      spec[2]:=specVec*vec3; if spec[2]>0 then spec[2]:=0;
      spec[0]:=spec[0]*spec[0]; spec[0]:=spec[0]*spec[0];
      spec[1]:=spec[1]*spec[1]; spec[1]:=spec[1]*spec[1];
      spec[2]:=spec[2]*spec[2]; spec[2]:=spec[2]*spec[2];
      result[0]:=colAmbient[0]+diffuse*colDiffuse[0]+spec[0]*spec[0]*0.5;
      result[1]:=colAmbient[1]+diffuse*colDiffuse[1]+spec[1]*spec[1]*0.5;
      result[2]:=colAmbient[2]+diffuse*colDiffuse[2]+spec[2]*spec[2]*0.5;
    end;

  VAR aid:double;
  begin
    if colorSource<9 then begin
      case colorVariant of
        0: aid:=                 rawData[colorSource mod 3] ;
        1: aid:=1-               rawData[colorSource mod 3] ;
        2: aid:=  system.sqr(1-2*rawData[colorSource mod 3]);
        3: aid:=1-system.sqr(1-2*rawData[colorSource mod 3]);
      end;
      aid:=system.exp(system.ln(aid)*pseudoGamma);
      case colorStyle of
        0: result:=fire(aid);
        1: result:=water(aid);
        2: result:=spectrum(aid);
        3: result:=trafficLight(aid);
        4: result:=earth(aid);
        5: result:=greyscale(aid);
        6: result:=zebra(aid);
        7: result:=greenzebra(aid);
        8: result:=rainbow(aid);
        9: result:=discrete(aid);
      end;
    end else begin
      case colorStyle of
        0: result:=normalToColor_metal  (rawData);
        1: result:=normalToColor_glass  (rawData);
        2: result:=normalToColor_plastic(rawData);
        3: result:=normalToColor_fire   (rawData);
        4: result:=normalToColor_drugged(rawData);
        5: result:=normalToColor_gold   (rawData);
        6: result:=normalToColor_levels (rawData);
        7: result:=normalToColor_strange(rawData);
        8: result:=normalToColor_window (rawData);
        9: result:=normalToColor_line   (rawData);
      end;
    end;
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.getColorAt(CONST ix,iy: longint; CONST xy: T_Complex): T_floatColor;
  begin
    result:=getColor(getRawDataAt(xy));
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.prepareRawMap(CONST yStart: longint);
  VAR x,y:longint;
      dat:T_floatColor;
  begin
    for y:=yStart to yStart do
    for x:=0 to temporaryRawMap^.width-1 do begin
      dat:=getRawDataAt(scaler.transform(x,y));
      temporaryRawMap^[x,y]:=dat;
      renderImage[x,y]:=getColor(dat);
    end;
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.prepareImage(CONST forPreview: boolean);
  VAR x,y:longint;
  FUNCTION todo(CONST y:longint):P_rawDataWorkerThreadTodo;
    begin new(result,create(@self,y)); end;

  begin
    if progressor.calculating then exit;
    scaler.rescale(renderImage.width,renderImage.height);
    if forPreview then begin
      rawMapIsOutdated:=rawMapIsOutdated or
                        (temporaryRawMap=nil) or
                        (temporaryRawMap^.width<>renderImage.width) or
                        (temporaryRawMap^.height<>renderImage.height);
      if temporaryRawMap=nil then new(temporaryRawMap,create(renderImage.width,renderImage.height));
      temporaryRawMap^.mutateType(rs_float);
      temporaryRawMap^.resize(renderImage.width,renderImage.height, res_dataResize);

      if rawMapIsOutdated then begin
        rawMapIsOutdated:=false;
        progressor.reset(et_stepCounter,renderImage.height);
        for y:=0 to renderImage.height-1 do queue.enqueue(todo(y));
      end else begin
        for y:=0 to renderImage.height-1 do for x:=0 to renderImage.width-1 do
        renderImage[x,y]:=getColor(temporaryRawMap^[x,y]);
      end;
    end else begin
      if temporaryRawMap<>nil then begin
        dispose(temporaryRawMap,destroy);
        temporaryRawMap:=nil;
      end;
      inherited prepareImage(false);
    end;
  end;

FUNCTION T_newton3Algorithm.getAlgorithmName: ansistring;
  begin
    result:='Newton(3)';
  end;

PROCEDURE T_newton3Algorithm.iterationStart(VAR c: T_Complex; OUT x: T_Complex);
  begin
    x:=c;
  end;

PROCEDURE T_newton3Algorithm.iterationStep(CONST c: T_Complex; VAR x: T_Complex);
  begin
    x:=(2/3)*x+1/(3*sqr(x));
  end;

VAR newton3Algorithm:T_newton3Algorithm;
INITIALIZATION
  newton3Algorithm.create; registerAlgorithm(@newton3Algorithm,true,true,false);
FINALIZATION
  newton3Algorithm.destroy;

end.

