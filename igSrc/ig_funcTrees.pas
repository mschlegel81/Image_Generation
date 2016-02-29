UNIT ig_funcTrees;
INTERFACE
USES imageGeneration,complex,myColors,myParams,myGenerics,sysutils,math,myFiles;
TYPE
  T_parameterSet=record
    operatorPos:array[0..3]      of T_Complex;
    c          :array[0..4]      of T_floatColor;
    node       :array[0..7,0..2] of T_Complex;
  end;

  T_legacyParameterSet=record
    operatorPos:array[0..3]      of record re,im:single; valid:boolean; end;
    c          :array[0..4]      of T_floatColor;
    node       :array[0..7,0..2] of record re,im:single; valid:boolean; end;
  end;

  { T_funcTree }

  T_funcTree=object(T_functionPerPixelAlgorithm)
    hueOffset,
    saturation,
    brightness:double;
    rotation:byte;
    par:T_parameterSet;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION parameterResetStyles:T_arrayOfString; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    FUNCTION getColorAt(CONST ix,iy:longint; CONST x:T_Complex):T_floatColor; virtual;
    PROCEDURE load(CONST fileName:ansistring);
  end;


IMPLEMENTATION

{ T_funcTree }

CONSTRUCTOR T_funcTree.create;
  CONST C_rotString:array[0..24] of string=(
      'none',              //0
      'rot2, origin, min', //1
      'rot2, center, min', //2
      'rot2, origin, avg', //3
      'rot2, center, avg', //4
      'rot2, origin, max', //5
      'rot2, center, max', //6
      'rot3, origin, min', //7
      'rot3, center, min', //8
      'rot3, origin, avg', //9
      'rot3, center, avg', //10
      'rot3, origin, max', //11
      'rot3, center, max', //12
      'rot4, origin, min', //13
      'rot4, center, min', //14
      'rot4, origin, avg', //15
      'rot4, center, avg', //16
      'rot4, origin, max', //17
      'rot4, center, max', //18
      'rot5, origin, min', //19
      'rot5, center, min', //20
      'rot5, origin, avg', //21
      'rot5, center, avg', //22
      'rot5, origin, max', //23
      'rot5, center, max');//24
  VAR i,j:longint;
  begin
    inherited create;
    {0} addParameter('rotation',pt_enum,0,25)^.setEnumValues(C_rotString);
    {1} addParameter('hue offset',pt_float,-1,1);
    {2} addParameter('saturation',pt_float,0);
    {3} addParameter('brightness',pt_float,0);
    {4..7} for i:=0 to 3 do addParameter('operatorPos['+intToStr(i)+']',pt_2floats);
    {8..13} for i:=0 to 4 do addParameter('color['+intToStr(i)+']',pt_color);
    for i:=0 to 7 do for j:=0 to 2 do addParameter('node['+intToStr(i)+','+intToStr(j)+']',pt_2floats);
    resetParameters(0);
  end;

DESTRUCTOR T_funcTree.destroy;
  begin end;

FUNCTION T_funcTree.getAlgorithmName: ansistring;
  begin
    result:='Function tree';
  end;

FUNCTION T_funcTree.parameterResetStyles: T_arrayOfString;
  begin
    result:='Reset (Zero)';
    append(result,'Randomize');
  end;

PROCEDURE T_funcTree.resetParameters(CONST style: longint);
  VAR i,j:longint;
  begin
    inherited resetParameters(style);
    with par do begin
      for i:=0 to 3 do operatorPos[i]:=style*((-1+2*random)+II*(-1+2*random));
      for i:=0 to 4 do c[i]:=style*newColor(random,random,random);
      for i:=0 to 7 do for j:=0 to 2 do node[i,j]:=style*((-1+2*random)+II*(-1+2*random));
    end;
    saturation:=1;
    brightness:=1;
    rotation:=0;
    hueOffset:=0;
  end;

FUNCTION T_funcTree.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+37;
  end;

PROCEDURE T_funcTree.setParameter(CONST index: byte; CONST value: T_parameterValue);
  VAR i,j:longint;
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else case byte(index-inherited numberOfParameters) of
      0: rotation:=value.i0;
      1: hueOffset:=value.f0;
      2: saturation:=value.f0;
      3: brightness:=value.f0;
      4..7: with par do begin
        i:=index-inherited numberOfParameters-4;
        operatorPos[i].re:=value.f0;
        operatorPos[i].im:=value.f1;
      end;
      8..12: with par do begin
        i:=index-inherited numberOfParameters-7;
        c[i]:=value.color; //toHSV(value.color);
      end;
      else with par do begin
        i:=index-inherited numberOfParameters-13;
        j:=i mod 3;
        i:=i div 3;
        node[i,j].re:=value.f0;
        node[i,j].im:=value.f1;
      end;
    end;
  end;

FUNCTION T_funcTree.getParameter(CONST index: byte): T_parameterValue;
  VAR i,j:longint;
  begin
    if index<inherited numberOfParameters then exit(inherited getParameter(index));
    case byte(index-inherited numberOfParameters) of
      0: result:=parValue(index,rotation);
      1: result:=parValue(index,hueOffset);
      2: result:=parValue(index,saturation);
      3: result:=parValue(index,brightness);
      4..7: with par do begin
        i:=index-inherited numberOfParameters-4;
        result:=parValue(index,operatorPos[i].re,operatorPos[i].im);
      end;
      8..12: with par do begin
        i:=index-inherited numberOfParameters-7;
        result:=parValue(index,c[i]);// parValue(index,fromHSV(c[i]));
      end;
      else with par do begin
        i:=index-inherited numberOfParameters-13;
        j:=i mod 3;
        i:=i div 3;
        result:=parValue(index,node[i,j].re,node[i,j].im);
      end;
    end;
  end;

FUNCTION T_funcTree.getColorAt(CONST ix, iy: longint; CONST x: T_Complex): T_floatColor;
  FUNCTION colorAt(CONST coord:T_Complex):T_floatColor;
    FUNCTION weightedOp(VAR x,y:T_Complex; w0,w1,w2,w3:single):T_Complex;
      begin
        w3:=w3*1/(0.1+sqrabs(y));
        result.re:=(x.re     +y.re     )*w0
                  +(x.re     -y.re     )*w1
                  +(x.re*y.re-x.im*y.im)*w2
                  +(x.im*y.re+x.re*y.im)*w3;
        result.im:=(x.im     +y.im     )*w0
                  +(x.im     -y.im     )*w1
                  +(x.re*y.im+x.re*y.im)*w2
                  +(x.im*y.re-x.re*y.im)*w3;
      end;

    VAR leaf      :array[0..7] of T_Complex;
        innerNode :array[0..6] of T_Complex;
        w         :array[0..6,0..3] of double;
        i,j       :longint;
        minDist   :double;
        tempNode6 :T_Complex;
    begin
      with par do for i:=0 to 6 do begin
        innerNode[i]:=node[i,0]+
                      node[i,1]*coord.re +
                      node[i,2]*coord.im;
        minDist:=system.exp(-0.18393972058572116*sqrabs(innerNode[i]));
        innerNode[i]:=innerNode[i]*minDist; //|innerNode[i]|<=1
        w[i,0]:=1-system.sqr(innerNode[i].re-operatorPos[0].re)+system.sqr(innerNode[i].im-operatorPos[0].im);
        w[i,1]:=1-system.sqr(innerNode[i].re-operatorPos[1].re)+system.sqr(innerNode[i].im-operatorPos[1].im);
        w[i,2]:=1-system.sqr(innerNode[i].re-operatorPos[2].re)+system.sqr(innerNode[i].im-operatorPos[2].im);
        w[i,3]:=1-system.sqr(innerNode[i].re-operatorPos[3].re)+system.sqr(innerNode[i].im-operatorPos[3].im);
        minDist:=1/(w[i,0]+w[i,1]+w[i,2]+w[i,3]);
        w[i,0]:=w[i,0]*minDist;
        w[i,1]:=w[i,1]*minDist;
        w[i,2]:=w[i,2]*minDist;
        w[i,3]:=w[i,3]*minDist;
      end;

      tempNode6:=coord;
      with par do for j:=0 to 3 do begin
        for i:=0 to 7 do leaf[(i+j) and 7]:=node[i,0]+node[i,1]*(tempNode6.re)+node[i,2]*(tempNode6.im);
        innerNode[0]:=weightedOp(leaf     [0],leaf     [1],w[0,0],w[0,1],w[0,2],w[0,3]);
        innerNode[1]:=weightedOp(leaf     [2],leaf     [3],w[1,0],w[1,1],w[1,2],w[1,3]);
        innerNode[2]:=weightedOp(innerNode[0],innerNode[1],w[2,0],w[2,1],w[2,2],w[2,3]);
        innerNode[3]:=weightedOp(leaf     [4],leaf     [5],w[3,0],w[3,1],w[3,2],w[3,3]);
        innerNode[4]:=weightedOp(innerNode[2],innerNode[3],w[4,0],w[4,1],w[4,2],w[4,3]);
        innerNode[5]:=weightedOp(leaf     [6],leaf     [7],w[5,0],w[5,1],w[5,2],w[5,3]);
        innerNode[6]:=weightedOp(innerNode[4],innerNode[5],w[6,0],w[6,1],w[6,2],w[6,3]);
        if isValid(innerNode[6]) then begin
          minDist:=system.exp(-0.18393972058572116*sqrabs(innerNode[6]));
          innerNode[6]:=innerNode[6]*minDist;
          tempNode6:=innerNode[6];
        end else break;
      end;
      with par do begin
        result:=c[0]+c[1]*(           tempNode6.re )
                    +c[2]*(           tempNode6.im )
                    +c[3]*(system.sqr(tempNode6.re))
                    +c[4]*(system.sqr(tempNode6.im));
        result[0]:=result[0]+hueOffset;
        result[1]:=result[1]*saturation;
        result[2]:=result[2]*brightness;
        result:=fromHSV(result);
      end;
    end;

  FUNCTION colMin(c1,c2:T_floatColor):T_floatColor;
    begin
      if c1[0]<c2[0] then result[0]:=c1[0] else result[0]:=c2[0];
      if c1[1]<c2[1] then result[1]:=c1[1] else result[1]:=c2[1];
      if c1[2]<c2[2] then result[2]:=c1[2] else result[2]:=c2[2];
    end;

  FUNCTION colMax(c1,c2:T_floatColor):T_floatColor;
    begin
      if c1[0]>c2[0] then result[0]:=c1[0] else result[0]:=c2[0];
      if c1[1]>c2[1] then result[1]:=c1[1] else result[1]:=c2[1];
      if c1[2]>c2[2] then result[2]:=c1[2] else result[2]:=c2[2];
    end;

  CONST rot72 :T_Complex=(re:system.cos(2*pi/5); im:system.sin(2*pi/5));
        rot90 :T_Complex=(re:0; im:1);
        rot120:T_Complex=(re:system.cos(2*pi/3); im:system.sin(2*pi/3));
        rot144:T_Complex=(re:system.cos(4*pi/5); im:system.sin(4*pi/5));
        rot216:T_Complex=(re:system.cos(6*pi/5); im:system.sin(6*pi/5));
        rot240:T_Complex=(re:system.cos(4*pi/3); im:system.sin(4*pi/3));
        rot270:T_Complex=(re:0; im:-1);
        rot288:T_Complex=(re:system.cos(8*pi/5); im:system.sin(8*pi/5));

  VAR c:T_Complex;
  begin
    if rotation in [2,4,6,8,10,12,14,16,18,20,22,24] then begin
      c.re:=scaler.getCenterX;
      c.im:=scaler.getCenterY;
    end else c:=0;

    case rotation of
       0: result:=colorAt(x);
       1: result:=colMin(colorAt(x),colorAt(-1*x   )); //'rot2, origin, min',
       2: result:=colMin(colorAt(x),colorAt(c-(x-c))); //'rot2, center, min',
       3: result:=0.5*  (colorAt(x)+colorAt(-1*x   ));//'rot2, origin, avg',
       4: result:=0.5*  (colorAt(x)+colorAt(c-(x-c)));//'rot2, center, avg',
       5: result:=colMax(colorAt(x),colorAt(-1*x   ));//'rot2, origin, max',
       6: result:=colMax(colorAt(x),colorAt(c-(x-c)));//'rot2, center, max',
       7: result:=colMin(colorAt(x),colMin(colorAt(   x   *rot120),colorAt(   x   *rot240))); //'rot3, origin, min',
       8: result:=colMin(colorAt(x),colMin(colorAt(c+(x-c)*rot120),colorAt(c+(x-c)*rot240)));//'rot3, center, min',
       9: result:=(1/3)*(colorAt(x)+       colorAt(   x   *rot120)+colorAt(   x   *rot240));
      10: result:=(1/3)*(colorAt(x)+       colorAt(c+(x-c)*rot120)+colorAt(c+(x-c)*rot240));
      11: result:=colMax(colorAt(x),colMax(colorAt(   x   *rot120),colorAt(   x   *rot240)));//'rot3, origin, max',
      12: result:=colMax(colorAt(x),colMax(colorAt(c+(x-c)*rot120),colorAt(c+(x-c)*rot240)));//'rot3, center, max',
      13: result:=colMin(colorAt(x),colMin(colorAt(   x   *rot90),colMin(colorAt(   x   *-1),colorAt(   x   *rot270))));
      14: result:=colMin(colorAt(x),colMin(colorAt(c+(x-c)*rot90),colMin(colorAt(c+(x-c)*-1),colorAt(c+(x-c)*rot270))));
      15: result:=0.25* (colorAt(x)       +colorAt(   x   *rot90)       +colorAt(   x   *-1)+colorAt(   x   *rot270));
      16: result:=0.25* (colorAt(x)       +colorAt(c+(x-c)*rot90)       +colorAt(c+(x-c)*-1)+colorAt(c+(x-c)*rot270));
      17: result:=colMax(colorAt(x),colMax(colorAt(   x   *rot90),colMax(colorAt(   x   *-1),colorAt(   x   *rot270))));
      18: result:=colMax(colorAt(x),colMax(colorAt(c+(x-c)*rot90),colMax(colorAt(c+(x-c)*-1),colorAt(c+(x-c)*rot270))));
      19: result:=colMin(colorAt(x),colMin(colorAt(   x   *rot72),colMin(colorAt(   x   *rot144),colMin(colorAt(   x   *rot216),colorAt(   x   *rot288)))));
      20: result:=colMin(colorAt(x),colMin(colorAt(c+(x-c)*rot72),colMin(colorAt(c+(x-c)*rot144),colMin(colorAt(c+(x-c)*rot216),colorAt(c+(x-c)*rot288)))));
      21: result:=0.2*  (colorAt(x)       +colorAt(   x   *rot72)       +colorAt(   x   *rot144)       +colorAt(   x   *rot216)+colorAt(   x   *rot288));
      22: result:=0.2*  (colorAt(x)       +colorAt(c+(x-c)*rot72)       +colorAt(c+(x-c)*rot144)       +colorAt(c+(x-c)*rot216)+colorAt(c+(x-c)*rot288));
      23: result:=colMax(colorAt(x),colMax(colorAt(   x   *rot72),colMax(colorAt(   x   *rot144),colMax(colorAt(   x   *rot216),colorAt(   x   *rot288)))));
      24: result:=colMax(colorAt(x),colMax(colorAt(c+(x-c)*rot72),colMax(colorAt(c+(x-c)*rot144),colMax(colorAt(c+(x-c)*rot216),colorAt(c+(x-c)*rot288)))));
    end;
  end;

PROCEDURE T_funcTree.load(CONST fileName:ansistring);
  VAR f:T_file;
      srot:single;
      param:T_legacyParameterSet;
      i,j:longint;
  begin
    if fileExists(fileName) then begin
      f.createToRead(fileName);
      scaler.setCenterX(f.readSingle);
      scaler.setCenterY(f.readSingle);
      scaler.setZoom(f.readSingle);
      f.readBuf(@param,sizeOf(param));
      hueOffset :=f.readSingle;
      srot      :=f.readSingle;
      rotation:=round(srot) mod 25;
      saturation:=f.readSingle;
      brightness:=f.readSingle;
      f.destroy;
    end;
    for i:=0 to 3 do par.operatorPos[i]:=param.operatorPos[i].re+II*param.operatorPos[i].im;
    for i:=0 to 4 do par.c[i]:=param.c[i];
    for i:=0 to 7 do for j:=0 to 2 do par.node[i,j]:=param.node[i,j].re+II*param.node[i,j].im;
  end;

VAR funcTree:T_funcTree;
INITIALIZATION
  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
  funcTree.create;
  registerAlgorithm(@funcTree,true,false,false);
FINALIZATION
  funcTree.destroy;
end.
