UNIT linAlg3d;
INTERFACE
USES math,sysutils,mypics;
CONST
  DIFF_REFLECTED=0;
  SPEC_REFLECTED=1;
  REFRACTED     =2;

  RAY_STEP_EPSILON=1E-10;
  NO_DIV_BY_ZERO_EPSILON=1E-10;

TYPE
  T_Vec3=array[0..2] of double;
  T_mat3x3=array[0..2,0..2] of double;

  FT_calcNodeCallback=FUNCTION (u,v:double):T_Vec3;

  T_boundingBox=object
    lower,upper:T_Vec3;
    CONSTRUCTOR createQuick;
    CONSTRUCTOR create(CONST x,y:T_Vec3);
    CONSTRUCTOR create(CONST x,y,z:T_Vec3);
    CONSTRUCTOR create(CONST center:T_Vec3; CONST radius:double);
    FUNCTION subBox(CONST i,j,k:shortint):T_boundingBox;
    DESTRUCTOR destroy;
    FUNCTION subBoxWithIndex(CONST p:T_Vec3; OUT i,j,k:shortint):T_boundingBox;
    FUNCTION intersects(CONST box:T_boundingBox):boolean;
    FUNCTION intersectsTriangle(CONST a,b,c:T_Vec3):boolean;
    FUNCTION intersectsSphere(CONST center:T_Vec3; CONST radius:double):boolean;
    FUNCTION intersectsSphereVolume(CONST cx,cy,cz,radius:double):boolean;
    FUNCTION contains(CONST p:T_Vec3):boolean;
    FUNCTION isInside(CONST box:T_boundingBox):boolean;
    PROCEDURE uniteWith(CONST box:T_boundingBox);
    PROCEDURE uniteWith(sample:T_Vec3);
    PROCEDURE expandToCube;
    FUNCTION diameter:double;
    FUNCTION center:T_Vec3;
    FUNCTION getCorner(CONST index:byte):T_Vec3;
  end;

  T_pointLightInstance=object
    pos,pseudoPos:T_Vec3;
    col:T_floatColor;
    infiniteDist:boolean;
    brightnessCap:double;

    CONSTRUCTOR create(p:T_Vec3; c:T_floatColor; infDist:boolean; cap:double);
    FUNCTION isRelevantAtPosition(position,normal:T_Vec3):boolean;
  end;

  T_ray=object
    start:T_Vec3;
    direction:T_Vec3;
    state:byte;

    CONSTRUCTOR createPrimary    (CONST startAt,dir:T_Vec3; CONST skip:double);
    CONSTRUCTOR createRefracted  (CONST startAt,dir:T_Vec3; CONST skip:double);
    CONSTRUCTOR createPathTracing(CONST startAt,dir:T_Vec3; CONST skip:double);
    CONSTRUCTOR createWithState  (CONST startAt,dir:T_Vec3; CONST skip:double; CONST rayState:byte);
    CONSTRUCTOR createLightScan  (CONST startAt,dir:T_Vec3; CONST skip:double; CONST lazy:boolean);
    DESTRUCTOR destroy;
    PROCEDURE modifyReflected(CONST normal:T_Vec3; CONST reflectDistortion:double);
  end;

  T_materialPoint=object
    position,
    normal:T_Vec3;
    hitTime:double;
    private
      localDiffuseColor,
      localFactor,
      reflectedFactor,
      refractedFactor:T_floatColor;
      reflectDistortion,relRefractionIdx,refractDistortion:double;
    public
      localGlowColor:T_floatColor;
      CONSTRUCTOR create(CONST pos,nrm:T_Vec3; CONST time:double; //hit point and normal;
                         CONST diffuse,glow,tranparency,reflectiveness:T_floatColor; //local colors
                         CONST reflectDist,refractDist,refracIdx:double); //local "indexes"
      DESTRUCTOR destroy;
      FUNCTION getLocalAmbientColor(CONST ambientExposure:double; CONST ambientLight:T_floatColor):T_floatColor;
      FUNCTION getColorAtPixel(CONST pointLight:T_pointLightInstance):T_floatColor;
      FUNCTION getLocal    (CONST c:T_floatColor):T_floatColor;
      FUNCTION getRefracted(CONST c:T_floatColor):T_floatColor;
      FUNCTION getReflected(CONST c:T_floatColor):T_floatColor;
      FUNCTION isReflective:boolean;
      FUNCTION isTransparent:boolean;
      FUNCTION getReflectDistortion:double;

      FUNCTION reflectRayAndReturnRefracted(VAR ray:T_ray):T_ray;
      PROCEDURE modifyReflectedRay(VAR ray:T_ray);
      FUNCTION getRayForLightScan(CONST rayState:byte):T_ray;
  end;

CONST
  RAY_STATE_PRIMARY          = 0;
  RAY_STATE_LIGHT_SCAN       = 1;
  RAY_STATE_LAZY_LIGHT_SCAN  = 2;
  RAY_STATE_PATH_TRACING     = 4;
  RAY_STATE_REFLECTED        = 8;
  RAY_STATE_REFRACTED        =16;

TYPE
  T_view=object
    xRes,yRes:longint;
    eyepoint,lookAt_:T_Vec3;
    eyeDistortion:double;
    lookDir :T_Vec3;
    up      :T_Vec3;
    right   :T_Vec3;

    CONSTRUCTOR create(screenWidth,screenHeight:longint; eye,lookat:T_Vec3; openingAngleInDegrees:double);
    PROCEDURE setLensDistortion(eyeSize:double; sharpAtDistance:double);
    PROCEDURE changeResolution(screenWidth,screenHeight:longint);
    FUNCTION getRay(CONST x,y:double):T_ray;
    PROCEDURE getYPlaneHitCoordinates(CONST screenX,screenY,worldY:double; OUT worldX,worldZ:double);
    DESTRUCTOR destroy;
  end;

  T_dynamicDarts=object
    private
      dart:array of T_Vec3;
      acceptanceRadius:double;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION current:T_Vec3;
      FUNCTION next:T_Vec3;
  end;


  P_node=^T_node;
  T_node=object
    position:T_Vec3;
    normal  :T_Vec3;
    CONSTRUCTOR create(calc:FT_calcNodeCallback; u,v:double);
    CONSTRUCTOR create(p,n:T_Vec3);
    DESTRUCTOR destroy;
  end;

  T_indexPair=array[0..1] of longint;

  T_nodeWithParam=record
                    n:P_node;
                    u,v:double;
                  end;
  T_edgeDef=record ni,fi: T_indexPair; len:double; end;

  T_faceDef=array[0..2] of longint;

  T_Graph = object
    surf:FT_calcNodeCallback;
    uPeriod,vPeriod:double;
    node: array of T_nodeWithParam;
    edge: array of T_edgeDef;
    face: array of T_faceDef;
    PROCEDURE distortEdge(e:longint);
    CONSTRUCTOR create(calc:FT_calcNodeCallback; u0,u1:double; uSteps:longint; v0,v1:double; vSteps:longint);

    DESTRUCTOR destroy;
    FUNCTION hasEdge(a, b: longint): longint;
    PROCEDURE updateMeetingFaces(VAR e:T_edgeDef);
    PROCEDURE updateMeetingFaces();
    FUNCTION edgeLength(ni0,ni1:longint):double;
    FUNCTION edgeLength(idx:longint):double;

    PROCEDURE optimizedNewNode(CONST node0,node1:T_nodeWithParam; OUT uNew,vNew:double);

    PROCEDURE orientFaces;
    FUNCTION faceOrientation(CONST a,b,c:T_nodeWithParam):double;
    FUNCTION  flipEdge(index:longint):boolean;

    FUNCTION triangleArea(index:longint):double;
    PROCEDURE addTriangle(i0, i1, i2: longint);
    FUNCTION addEdge(node0,node1   : longint):longint;
    FUNCTION addEdge(node0,node1,face0,face1: longint):longint;
    PROCEDURE splitEdgesLongerThan(threshold:double);
    PROCEDURE splitTryingToObtainFaceCount(faceCount:longint);
    PROCEDURE writeReport;
    PROCEDURE writeShortReport;
    FUNCTION trianglesWithNonzeroArea:longint;
  end;

CONST
  zeroVec:T_Vec3=(0,0,0);
  unitMat:T_mat3x3=((1,0,0),(0,1,0),(0,0,1));

OPERATOR +(x,y:T_Vec3):T_Vec3;
OPERATOR -(x,y:T_Vec3):T_Vec3;
OPERATOR *(x,y:T_Vec3):double;
OPERATOR *(x:T_Vec3; y:double):T_Vec3;
OPERATOR *(x:double; y:T_Vec3):T_Vec3;
FUNCTION newVector(x,y,z:double):T_Vec3;
FUNCTION randomVecOnUnitSphere:T_Vec3;
FUNCTION randomVecInUnitSphere:T_Vec3;
FUNCTION cross(x,y:T_Vec3):T_Vec3;
FUNCTION norm(x:T_Vec3):double;
FUNCTION sqNorm(x:T_Vec3):double;
FUNCTION normed(x:T_Vec3):T_Vec3;
FUNCTION rotX(alpha:double; v:T_Vec3):T_Vec3;

FUNCTION newColMat(x,y,z:T_Vec3):T_mat3x3;
FUNCTION newRowMat(x,y,z:T_Vec3):T_mat3x3;
OPERATOR *(x:T_mat3x3; y:T_Vec3):T_Vec3;
OPERATOR *(x,y:T_mat3x3):T_mat3x3;
FUNCTION inverse(B:T_mat3x3):T_mat3x3;

FUNCTION solveSystemRowVec(bx,by,bz,a:T_Vec3):T_Vec3;
FUNCTION solveSystemColVec(bx,by,bz,a:T_Vec3):T_Vec3;

FUNCTION triangleCutsBox(CONST t0,t1,t2,lower,upper:T_Vec3):boolean;



IMPLEMENTATION
OPERATOR +(x,y:T_Vec3):T_Vec3; VAR i:byte; begin for i:=0 to 2 do result[i]:=x[i]+y[i]; end;
OPERATOR -(x,y:T_Vec3):T_Vec3; VAR i:byte; begin for i:=0 to 2 do result[i]:=x[i]-y[i]; end;
OPERATOR *(x,y:T_Vec3):double; begin result:=x[0]*y[0]+x[1]*y[1]+x[2]*y[2]; end;
OPERATOR *(x:T_Vec3; y:double):T_Vec3; VAR i:byte; begin for i:=0 to 2 do result[i]:=x[i]*y; end;
OPERATOR *(x:double; y:T_Vec3):T_Vec3; VAR i:byte; begin for i:=0 to 2 do result[i]:=x*y[i]; end;
FUNCTION cross(x,y:T_Vec3):T_Vec3;
  begin
    result[0]:=x[1]*y[2]-x[2]*y[1];
    result[1]:=x[2]*y[0]-x[0]*y[2];
    result[2]:=x[0]*y[1]-x[1]*y[0];
  end;

FUNCTION norm(x:T_Vec3):double;
  begin
    result:=sqrt(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]);
  end;

FUNCTION sqNorm(x:T_Vec3):double;
  begin
    result:=x[0]*x[0]+x[1]*x[1]+x[2]*x[2];
  end;


FUNCTION normed(x:T_Vec3):T_Vec3;
  VAR fac:double;
  begin
    fac:=(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]);
    if fac>0 then begin
      fac:=1/sqrt(fac);
      result[0]:=fac*x[0];
      result[1]:=fac*x[1];
      result[2]:=fac*x[2];
    end else result:=zeroVec;
  end;

FUNCTION newColMat(x,y,z:T_Vec3):T_mat3x3;
  VAR i:longint;
  begin
    for i:=0 to 2 do begin result[i,0]:=x[i]; result[i,1]:=y[i]; result[i,2]:=z[i]; end;
  end;

FUNCTION newRowMat(x,y,z:T_Vec3):T_mat3x3;
  begin
    result[0]:=x; result[1]:=y; result[2]:=z;
  end;

OPERATOR *(x:T_mat3x3; y:T_Vec3):T_Vec3;
  begin
    result[0]:=x[0,0]*y[0]+x[0,1]*y[1]+x[0,2]*y[2];
    result[1]:=x[1,0]*y[0]+x[1,1]*y[1]+x[1,2]*y[2];
    result[2]:=x[2,0]*y[0]+x[2,1]*y[1]+x[2,2]*y[2];
  end;

OPERATOR *(x,y:T_mat3x3):T_mat3x3;
  VAR i,j:longint;
  begin
    for i:=0 to 2 do for j:=0 to 2 do
    result[i,j]:=x[i,0]*y[0,j]+x[i,1]*y[1,j]+x[i,2]*y[2,j];
  end;

FUNCTION inverse(B:T_mat3x3):T_mat3x3;
  VAR invDet:double;
  begin
    invDet:=(b[0,0]*b[1,1]*b[2,2]
            +b[0,1]*b[1,2]*b[2,0]
            +b[0,2]*b[1,0]*b[2,1]
            -b[0,0]*b[1,2]*b[2,1]
            -b[0,1]*b[1,0]*b[2,2]
            -b[0,2]*b[1,1]*b[2,0]);
    if abs(invDet)>1E-10 then begin
      invDet:=1/invDet;
      result[0,0]:=invDet*(b[1,1]*b[2,2]-b[1,2]*b[2,1]);
      result[1,0]:=invDet*(b[1,2]*b[2,0]-b[1,0]*b[2,2]);
      result[2,0]:=invDet*(b[1,0]*b[2,1]-b[1,1]*b[2,0]);
      result[0,1]:=invDet*(b[2,1]*b[0,2]-b[2,2]*b[0,1]);
      result[1,1]:=invDet*(b[2,2]*b[0,0]-b[2,0]*b[0,2]);
      result[2,1]:=invDet*(b[2,0]*b[0,1]-b[2,1]*b[0,0]);
      result[0,2]:=invDet*(b[0,1]*b[1,2]-b[0,2]*b[1,1]);
      result[1,2]:=invDet*(b[0,2]*b[1,0]-b[0,0]*b[1,2]);
      result[2,2]:=invDet*(b[0,0]*b[1,1]-b[0,1]*b[1,0]);
    end else begin
      result[0,0]:=Nan;
      result[1,0]:=Nan;
      result[2,0]:=Nan;
      result[0,1]:=Nan;
      result[1,1]:=Nan;
      result[2,1]:=Nan;
      result[0,2]:=Nan;
      result[1,2]:=Nan;
      result[2,2]:=Nan;
    end;
  end;

FUNCTION solveSystemRowVec(bx,by,bz,a:T_Vec3):T_Vec3;
  VAR invDet:double;
  begin
    invDet:=1/(bx[0]*by[1]*bz[2]
              +bx[1]*by[2]*bz[0]
              +bx[2]*by[0]*bz[1]
              -bx[0]*by[2]*bz[1]
              -bx[1]*by[0]*bz[2]
              -bx[2]*by[1]*bz[0]);
    result[0]:=invDet*(a[0]*(by[1]*bz[2]-by[2]*bz[1])
                      +a[1]*(bz[1]*bx[2]-bz[2]*bx[1])
                      +a[2]*(bx[1]*by[2]-bx[2]*by[1]));
    result[1]:=invDet*(a[0]*(by[2]*bz[0]-by[0]*bz[2])
                      +a[1]*(bz[2]*bx[0]-bz[0]*bx[2])
                      +a[2]*(bx[2]*by[0]-bx[0]*by[2]));
    result[2]:=invDet*(a[0]*(by[0]*bz[1]-by[1]*bz[0])
                      +a[1]*(bz[0]*bx[1]-bz[1]*bx[0])
                      +a[2]*(bx[0]*by[1]-bx[1]*by[0]));
  end;

FUNCTION solveSystemColVec(bx,by,bz,a:T_Vec3):T_Vec3;
  VAR invDet:double;
  begin
    invDet:=(bx[0]*by[1]*bz[2]
            +by[0]*bz[1]*bx[2]
            +bz[0]*bx[1]*by[2]
            -bx[0]*bz[1]*by[2]
            -by[0]*bx[1]*bz[2]
            -bz[0]*by[1]*bx[2]);
    if (abs(invDet)>NO_DIV_BY_ZERO_EPSILON) then begin
      invDet:=1/invDet;
      result[0]:=invDet*(a[0]*(by[1]*bz[2]-bz[1]*by[2])
                        +a[1]*(by[2]*bz[0]-bz[2]*by[0])
                        +a[2]*(by[0]*bz[1]-bz[0]*by[1]));
      result[1]:=invDet*(a[0]*(bz[1]*bx[2]-bx[1]*bz[2])
                        +a[1]*(bz[2]*bx[0]-bx[2]*bz[0])
                        +a[2]*(bz[0]*bx[1]-bx[0]*bz[1]));
      result[2]:=invDet*(a[0]*(bx[1]*by[2]-by[1]*bx[2])
                        +a[1]*(bx[2]*by[0]-by[2]*bx[0])
                        +a[2]*(bx[0]*by[1]-by[0]*bx[1]));
    end else begin
      result[0]:=Nan;
      result[1]:=Nan;
      result[2]:=Nan;
    end;
  end;

FUNCTION newVector(x,y,z:double):T_Vec3;
  begin
    result[0]:=x; result[1]:=y; result[2]:=z;
  end;

FUNCTION randomVecOnUnitSphere:T_Vec3;
  VAR a:double;
  begin
    repeat
      result[0]:=random-0.5;
      result[1]:=random-0.5;
      result[2]:=random-0.5;
      a:=result[0]*result[0]+
         result[1]*result[1]+
         result[2]*result[2];
    until (a<0.25) and (a>NO_DIV_BY_ZERO_EPSILON);
    a:=1/sqrt(a);
    result[0]:=result[0]*a;
    result[1]:=result[1]*a;
    result[2]:=result[2]*a;
  end;

FUNCTION randomVecInUnitSphere:T_Vec3;
  VAR a:double;
  begin
    repeat
      result[0]:=random-0.5;
      result[1]:=random-0.5;
      result[2]:=random-0.5;
      a:=result[0]*result[0]+
         result[1]*result[1]+
         result[2]*result[2];
    until (a<0.25) and (a>NO_DIV_BY_ZERO_EPSILON);
    result[0]:=result[0]*2;
    result[1]:=result[1]*2;
    result[2]:=result[2]*2;
  end;


FUNCTION rotX(alpha:double; v:T_Vec3):T_Vec3;
  VAR c,s:single;
  begin
    c:=system.cos(alpha);
    s:=system.sin(alpha);
    result[0]:=v[0];
    result[1]:=v[1]*c+v[2]*s;
    result[2]:=v[2]*c-v[1]*s;
  end;

FUNCTION triangleCutsBox(CONST t0,t1,t2,lower,upper:T_Vec3):boolean;
  VAR b0,b1,b2:byte;
      normal:T_Vec3;
      d0,d1:double;

  begin
    if t0[0]<lower[0] then     b0:=1  else if t0[0]>upper[0] then     b0:=2 else b0:=0;
    if t0[1]<lower[1] then inc(b0, 4) else if t0[1]>upper[1] then inc(b0, 8);
    if t0[2]<lower[2] then inc(b0,16) else if t0[2]>upper[2] then inc(b0,32);
    if b0=0 then exit(true); //fast acceptance: at least one node (that is t0) is in box
    if t1[0]<lower[0] then     b1:=1  else if t1[0]>upper[0] then     b1:=2 else b1:=0;
    if t1[1]<lower[1] then inc(b1, 4) else if t1[1]>upper[1] then inc(b1, 8);
    if t1[2]<lower[2] then inc(b1,16) else if t1[2]>upper[2] then inc(b1,32);
    if b1=0 then exit(true); //fast acceptance: at least one node (that is t1) is in box
    if t2[0]<lower[0] then     b2:=1  else if t2[0]>upper[0] then     b2:=2 else b2:=0;
    if t2[1]<lower[1] then inc(b2, 4) else if t2[1]>upper[1] then inc(b2, 8);
    if t2[2]<lower[2] then inc(b2,16) else if t2[2]>upper[2] then inc(b2,32);
    if b2=0 then exit(true); //fast acceptance: at least one node (that is t2) is in box
    if (b0 and b1 and b2>0) then exit(false); //fast rejection: all nodes are outside "in the same direction"
    //fast acceptance: there are two nodes "opposite to each other"
    if (b0 or b1) in [3,12,15,48,51,60,63] then begin exit(true); end;
    if (b1 or b2) in [3,12,15,48,51,60,63] then begin exit(true); end;
    if (b2 or b0) in [3,12,15,48,51,60,63] then begin exit(true); end;
    //calculate normal of the triangle; if all box nodes lie on the same side, there is no intersection
    normal:=cross(t1-t0,t2-t0);
    d0:=(lower-t0)*normal;
    normal:=(upper[0]-lower[0])*normal;
    d1:=d0                              ; if d1>0 then b0:=1 else b0:=0; if d1<0 then b1:=1 else b1:=0;
    d1:=d0                    +normal[2]; if d1>0 then inc(b0,  2);      if d1<0 then inc(b1,  2);
    d1:=d0          +normal[1]          ; if d1>0 then inc(b0,  4);      if d1<0 then inc(b1,  4);
    d1:=d0          +normal[1]+normal[2]; if d1>0 then inc(b0,  8);      if d1<0 then inc(b1,  8);
    d1:=d0+normal[0]                    ; if d1>0 then inc(b0, 16);      if d1<0 then inc(b1, 16);
    d1:=d0+normal[0]          +normal[2]; if d1>0 then inc(b0, 32);      if d1<0 then inc(b1, 32);
    d1:=d0+normal[0]+normal[1]          ; if d1>0 then inc(b0, 64);      if d1<0 then inc(b1, 64);
    d1:=d0+normal[0]+normal[1]+normal[2]; if d1>0 then inc(b0,128);      if d1<0 then inc(b1,128);
    result:=(b0<>0) or (b1<>0);
  end;

CONSTRUCTOR T_boundingBox.createQuick;
  begin
    lower:=zeroVec;
    upper:=zeroVec;
  end;

CONSTRUCTOR T_boundingBox.create(CONST x,y:T_Vec3);
  begin
    lower[0]:=min(x[0],y[0]);
    lower[1]:=min(x[1],y[1]);
    lower[2]:=min(x[2],y[2]);
    upper[0]:=max(x[0],y[0]);
    upper[1]:=max(x[1],y[1]);
    upper[2]:=max(x[2],y[2]);
  end;

CONSTRUCTOR T_boundingBox.create(CONST x,y,z:T_Vec3);
  begin
    lower[0]:=min(min(x[0],y[0]),z[0]);
    lower[1]:=min(min(x[1],y[1]),z[1]);
    lower[2]:=min(min(x[2],y[2]),z[2]);
    upper[0]:=max(max(x[0],y[0]),z[0]);
    upper[1]:=max(max(x[1],y[1]),z[1]);
    upper[2]:=max(max(x[2],y[2]),z[2]);
  end;

CONSTRUCTOR T_boundingBox.create(CONST center:T_Vec3; CONST radius:double);
  begin
    lower[0]:=center[0]-radius;
    lower[1]:=center[1]-radius;
    lower[2]:=center[2]-radius;
    upper[0]:=center[0]+radius;
    upper[1]:=center[1]+radius;
    upper[2]:=center[2]+radius;
  end;

FUNCTION T_boundingBox.subBox(CONST i,j,k:shortint):T_boundingBox;
  VAR mid:T_Vec3;
  begin
    mid:=(lower+upper)*0.5;
    result.createQuick;
    if i=0 then begin result.lower[0]:=lower[0]; result.upper[0]:=mid  [0] end
           else begin result.lower[0]:=mid  [0]; result.upper[0]:=upper[0] end;
    if j=0 then begin result.lower[1]:=lower[1]; result.upper[1]:=mid  [1] end
           else begin result.lower[1]:=mid  [1]; result.upper[1]:=upper[1] end;
    if k=0 then begin result.lower[2]:=lower[2]; result.upper[2]:=mid  [2] end
           else begin result.lower[2]:=mid  [2]; result.upper[2]:=upper[2] end;
  end;

DESTRUCTOR T_boundingBox.destroy;
  begin
  end;

FUNCTION T_boundingBox.subBoxWithIndex(CONST p:T_Vec3; OUT i,j,k:shortint):T_boundingBox;
  VAR mid:T_Vec3;
  begin
    mid:=(lower+upper)*0.5;
    result.createQuick;
    if p[0]<mid[0] then begin result.lower[0]:=lower[0]; result.upper[0]:=mid[0]; i:=0; end
                   else begin result.lower[0]:=mid[0]; result.upper[0]:=upper[0]; i:=1; end;
    if p[1]<mid[1] then begin result.lower[1]:=lower[1]; result.upper[1]:=mid[1]; j:=0; end
                   else begin result.lower[1]:=mid[1]; result.upper[1]:=upper[1]; j:=1; end;
    if p[2]<mid[2] then begin result.lower[2]:=lower[2]; result.upper[2]:=mid[2]; k:=0; end
                   else begin result.lower[2]:=mid[2]; result.upper[2]:=upper[2]; k:=1; end;
  end;

FUNCTION T_boundingBox.intersects(CONST box:T_boundingBox):boolean;
  begin
    result:=(lower[0]<=box.upper[0]) and (upper[0]>=box.lower[0])
        and (lower[1]<=box.upper[1]) and (upper[1]>=box.lower[1])
        and (lower[2]<=box.upper[2]) and (upper[2]>=box.lower[2]);
  end;

FUNCTION T_boundingBox.intersectsTriangle(CONST a,b,c:T_Vec3):boolean;
  VAR b0,b1,b2:byte;
      normal:T_Vec3;
      d0,d1:double;
  begin
    if a[0]<lower[0] then     b0:=1  else if a[0]>upper[0] then     b0:=2 else b0:=0;
    if a[1]<lower[1] then inc(b0, 4) else if a[1]>upper[1] then inc(b0, 8);
    if a[2]<lower[2] then inc(b0,16) else if a[2]>upper[2] then inc(b0,32);
    if b0=0 then exit(true); //fast acceptance: at least one node (that is a) is in box
    if b[0]<lower[0] then     b1:=1  else if b[0]>upper[0] then     b1:=2 else b1:=0;
    if b[1]<lower[1] then inc(b1, 4) else if b[1]>upper[1] then inc(b1, 8);
    if b[2]<lower[2] then inc(b1,16) else if b[2]>upper[2] then inc(b1,32);
    if b1=0 then exit(true); //fast acceptance: at least one node (that is b) is in box
    if c[0]<lower[0] then     b2:=1  else if c[0]>upper[0] then     b2:=2 else b2:=0;
    if c[1]<lower[1] then inc(b2, 4) else if c[1]>upper[1] then inc(b2, 8);
    if c[2]<lower[2] then inc(b2,16) else if c[2]>upper[2] then inc(b2,32);
    if b2=0 then exit(true); //fast acceptance: at least one node (that is c) is in box
    if (b0 and b1 and b2>0) then exit(false); //fast rejection: all nodes are outside "in the same direction"
    //fast acceptance: there are two nodes "opposite to each other"
    if (b0 or b1) in [3,12,15,48,51,60,63] then begin exit(true); end;
    if (b1 or b2) in [3,12,15,48,51,60,63] then begin exit(true); end;
    if (b2 or b0) in [3,12,15,48,51,60,63] then begin exit(true); end;
    //calculate normal of the triangle; if all box nodes lie on the same side, there is no intersection
    normal:=cross(b-a,c-a);
    d0:=(lower-a)*normal;
    normal:=(upper[0]-lower[0])*normal;
    d1:=d0                              ; if d1>0 then b0:=1 else b0:=0; if d1<0 then b1:=1 else b1:=0;
    d1:=d0                    +normal[2]; if d1>0 then inc(b0,  2);      if d1<0 then inc(b1,  2);
    d1:=d0          +normal[1]          ; if d1>0 then inc(b0,  4);      if d1<0 then inc(b1,  4);
    d1:=d0          +normal[1]+normal[2]; if d1>0 then inc(b0,  8);      if d1<0 then inc(b1,  8);
    d1:=d0+normal[0]                    ; if d1>0 then inc(b0, 16);      if d1<0 then inc(b1, 16);
    d1:=d0+normal[0]          +normal[2]; if d1>0 then inc(b0, 32);      if d1<0 then inc(b1, 32);
    d1:=d0+normal[0]+normal[1]          ; if d1>0 then inc(b0, 64);      if d1<0 then inc(b1, 64);
    d1:=d0+normal[0]+normal[1]+normal[2]; if d1>0 then inc(b0,128);      if d1<0 then inc(b1,128);
    result:=(b0<>0) or (b1<>0);
  end;


FUNCTION T_boundingBox.intersectsSphere(CONST center:T_Vec3; CONST radius:double):boolean;
  VAR sample:T_Vec3;
  begin
    if (center[0]>=lower[0]) and (center[0]<=upper[0]) and
       (center[1]>=lower[1]) and (center[1]<=upper[1]) and
       (center[2]>=lower[2]) and (center[2]<=upper[2]) then exit(true);
    if      center[0]<=lower[0] then sample[0]:=lower[0]-center[0]
    else if center[0]>=upper[0] then sample[0]:=upper[0]-center[0]
    else                             sample[0]:=0;
    if      center[1]<=lower[1] then sample[1]:=lower[1]-center[1]
    else if center[1]>=upper[1] then sample[1]:=upper[1]-center[1]
    else                             sample[1]:=0;
    if      center[2]<=lower[2] then sample[2]:=lower[2]-center[2]
    else if center[2]>=upper[2] then sample[2]:=upper[2]-center[2]
    else                             sample[2]:=0;
    result:=(sample[0]*sample[0]+sample[1]*sample[1]+sample[2]*sample[2]<=radius*radius)
        and ((sqr(lower[0]-center[0])+sqr(lower[1]-center[1])+sqr(lower[2]-center[2])>radius*radius)
          or (sqr(upper[0]-center[0])+sqr(lower[1]-center[1])+sqr(lower[2]-center[2])>radius*radius)
          or (sqr(lower[0]-center[0])+sqr(upper[1]-center[1])+sqr(lower[2]-center[2])>radius*radius)
          or (sqr(upper[0]-center[0])+sqr(upper[1]-center[1])+sqr(lower[2]-center[2])>radius*radius)
          or (sqr(lower[0]-center[0])+sqr(lower[1]-center[1])+sqr(upper[2]-center[2])>radius*radius)
          or (sqr(upper[0]-center[0])+sqr(lower[1]-center[1])+sqr(upper[2]-center[2])>radius*radius)
          or (sqr(lower[0]-center[0])+sqr(upper[1]-center[1])+sqr(upper[2]-center[2])>radius*radius)
          or (sqr(upper[0]-center[0])+sqr(upper[1]-center[1])+sqr(upper[2]-center[2])>radius*radius));
  end;

FUNCTION T_boundingBox.intersectsSphereVolume(CONST cx,cy,cz,radius:double):boolean;
  VAR d:double;
  begin
    if      cx<=lower[0] then d:=  sqr(lower[0]-cx)
    else if cx>=upper[0] then d:=  sqr(upper[0]-cx) else d:=0;
    if      cy<=lower[1] then d:=d+sqr(lower[1]-cy)
    else if cy>=upper[1] then d:=d+sqr(upper[1]-cy);
    if      cz<=lower[2] then d:=d+sqr(lower[2]-cz)
    else if cz>=upper[2] then d:=d+sqr(upper[2]-cz);
    result:=(d<=radius*radius);
  end;

FUNCTION T_boundingBox.contains(CONST p:T_Vec3):boolean;
  begin
    result:= (lower[0]<=p[0]) and (p[0]<=upper[0]) and
             (lower[1]<=p[1]) and (p[1]<=upper[1]) and
             (lower[2]<=p[2]) and (p[2]<=upper[2]);
  end;


FUNCTION T_boundingBox.isInside(CONST box:T_boundingBox):boolean;
  begin
    result:=(lower[0]>=box.lower[0]) and (upper[0]<=box.upper[0])
        and (lower[1]>=box.lower[1]) and (upper[1]<=box.upper[1])
        and (lower[2]>=box.lower[2]) and (upper[2]<=box.upper[2]);
  end;

PROCEDURE T_boundingBox.uniteWith(CONST box:T_boundingBox);
  begin
    lower[0]:=min(lower[0],box.lower[0]);
    lower[1]:=min(lower[1],box.lower[1]);
    lower[2]:=min(lower[2],box.lower[2]);
    upper[0]:=max(upper[0],box.upper[0]);
    upper[1]:=max(upper[1],box.upper[1]);
    upper[2]:=max(upper[2],box.upper[2]);
  end;

PROCEDURE T_boundingBox.uniteWith(sample:T_Vec3);
  begin
    lower[0]:=min(lower[0],sample[0]);
    lower[1]:=min(lower[1],sample[1]);
    lower[2]:=min(lower[2],sample[2]);
    upper[0]:=max(upper[0],sample[0]);
    upper[1]:=max(upper[1],sample[1]);
    upper[2]:=max(upper[2],sample[2]);
  end;

PROCEDURE T_boundingBox.expandToCube;
  VAR len:double;
  begin
    len:=max(upper[0]-lower[0],max(upper[1]-lower[1],upper[2]-lower[2]));
    upper:=lower+newVector(len,len,len);
  end;

FUNCTION T_boundingBox.diameter:double;
  begin
    result:=sqrt(sqr(upper[0]-lower[0])+sqr(upper[1]-lower[1])+sqr(upper[2]-lower[2]));
  end;

FUNCTION T_boundingBox.center:T_Vec3;
  begin
    result[0]:=0.5*(lower[0]+upper[0]);
    result[1]:=0.5*(lower[1]+upper[1]);
    result[2]:=0.5*(lower[2]+upper[2]);
  end;

FUNCTION T_boundingBox.getCorner(CONST index:byte):T_Vec3;
  begin
    if (index and 1)=0 then result[0]:=lower[0] else result[0]:=upper[0];
    if (index and 2)=0 then result[1]:=lower[1] else result[1]:=upper[1];
    if (index and 4)=0 then result[2]:=lower[2] else result[2]:=upper[2];
  end;


CONSTRUCTOR T_pointLightInstance.create(p:T_Vec3; c:T_floatColor; infDist:boolean; cap:double);
  begin pos:=p; pseudoPos:=p; col:=c; infiniteDist:=infDist; brightnessCap:=cap; end;

FUNCTION T_pointLightInstance.isRelevantAtPosition(position,normal:T_Vec3):boolean;
  begin
    if infiniteDist
      then result:=(normal*pos*max(col[0],max(col[1],col[2]))>0.001)
      else result:=((normal*(pos-position))*max(col[0],max(col[1],col[2]))/sqNorm(position-pos)>0.001);
  end;

CONSTRUCTOR T_materialPoint.create(CONST pos,nrm:T_Vec3; CONST time:double; //hit point and normal;
                                   CONST diffuse,glow,tranparency,reflectiveness:T_floatColor; //local colors
                                   CONST reflectDist,refractDist,refracIdx:double); //local "indexes"
  begin
    position:=pos;
    normal  :=nrm;
    hitTime :=time;
    localFactor    :=newColor((1-reflectiveness[0])*(1-tranparency[0]),
                              (1-reflectiveness[1])*(1-tranparency[1]),
                              (1-reflectiveness[2])*(1-tranparency[2]));
    localGlowColor[0]:=glow[0]*localFactor[0];
    localGlowColor[1]:=glow[1]*localFactor[1];
    localGlowColor[2]:=glow[2]*localFactor[2];
    localDiffuseColor[0]:=diffuse[0]*localFactor[0];
    localDiffuseColor[1]:=diffuse[1]*localFactor[1];
    localDiffuseColor[2]:=diffuse[2]*localFactor[2];

    reflectedFactor:=newColor(   reflectiveness[0] *(1-tranparency[0]),
                                 reflectiveness[1] *(1-tranparency[1]),
                                 reflectiveness[2] *(1-tranparency[2]));
    refractedFactor:=tranparency;
    reflectDistortion:=reflectDist;
    relRefractionIdx:=refracIdx;
    refractDistortion:=refractDist;
  end;

DESTRUCTOR T_materialPoint.destroy; begin end;

FUNCTION T_materialPoint.getLocalAmbientColor(CONST ambientExposure:double; CONST ambientLight:T_floatColor):T_floatColor;
  begin
    result[0]:=ambientLight[0]*localDiffuseColor[0]*ambientExposure;
    result[1]:=ambientLight[1]*localDiffuseColor[1]*ambientExposure;
    result[2]:=ambientLight[2]*localDiffuseColor[2]*ambientExposure;
  end;

FUNCTION T_materialPoint.getLocal    (CONST c:T_floatColor):T_floatColor;
  begin
    result[0]:=c[0]*localFactor[0];
    result[1]:=c[1]*localFactor[1];
    result[2]:=c[2]*localFactor[2];
  end;

FUNCTION T_materialPoint.getColorAtPixel(CONST pointLight:T_pointLightInstance):T_floatColor;
  VAR aid,factor:double;
      lightDir:T_Vec3;

  begin
    if pointLight.infiniteDist then begin
      factor:=1;
      lightDir:=pointLight.pseudoPos;
    end else begin
      lightDir:=pointLight.pos-position;
      factor:=1/sqNorm(lightDir);
      lightDir:=lightDir*sqrt(factor);
      if factor>pointLight.brightnessCap then factor:=pointLight.brightnessCap;
    end;

    //diffuse part:--------------------------------
    aid:=(normal*lightDir)*factor;
    if aid<=0 then result:=black else begin
      result[0]:=aid*pointLight.col[0]*localDiffuseColor[0];
      result[1]:=aid*pointLight.col[1]*localDiffuseColor[1];
      result[2]:=aid*pointLight.col[2]*localDiffuseColor[2];
    end;
    //--------------------------------:diffuse part
  end;

FUNCTION T_materialPoint.getRefracted(CONST c:T_floatColor):T_floatColor;
  begin
    result[0]:=c[0]*refractedFactor[0];
    result[1]:=c[1]*refractedFactor[1];
    result[2]:=c[2]*refractedFactor[2];
  end;

FUNCTION T_materialPoint.getReflected(CONST c:T_floatColor):T_floatColor;
  begin
    result[0]:=c[0]*reflectedFactor[0];
    result[1]:=c[1]*reflectedFactor[1];
    result[2]:=c[2]*reflectedFactor[2];
  end;

FUNCTION T_materialPoint.isReflective:boolean;
  begin
    result:=(reflectedFactor[0]>1E-2) or
            (reflectedFactor[1]>1E-2) or
            (reflectedFactor[2]>1E-2);
  end;

FUNCTION T_materialPoint.isTransparent:boolean;
  begin
    result:=(refractedFactor[0]>1E-2) or
            (refractedFactor[1]>1E-2) or
            (refractedFactor[2]>1E-2);
  end;

FUNCTION T_materialPoint.getReflectDistortion:double;
  begin
    result:=reflectDistortion;
  end;

CONSTRUCTOR T_ray.createPrimary    (CONST startAt,dir:T_Vec3; CONST skip:double);
  begin
    state:=RAY_STATE_PRIMARY;
    start:=startAt+skip*dir;
    direction:=dir;
  end;

CONSTRUCTOR T_ray.createRefracted  (CONST startAt,dir:T_Vec3; CONST skip:double);
  begin
    state:=RAY_STATE_REFRACTED;
    start:=startAt+skip*dir;
    direction:=dir;
  end;

CONSTRUCTOR T_ray.createWithState(CONST startAt,dir:T_Vec3; CONST skip:double; CONST rayState:byte);
  begin
    state:=rayState;
    start:=startAt+skip*dir;
    direction:=dir;
  end;

CONSTRUCTOR T_ray.createPathTracing(CONST startAt,dir:T_Vec3; CONST skip:double);
  begin
    state:=RAY_STATE_PATH_TRACING;
    start:=startAt+skip*dir;
    direction:=dir;
  end;

CONSTRUCTOR T_ray.createLightScan  (CONST startAt,dir:T_Vec3; CONST skip:double; CONST lazy:boolean);
  begin
    if lazy then state:=RAY_STATE_LAZY_LIGHT_SCAN
            else state:=RAY_STATE_LIGHT_SCAN;
    start:=startAt+skip*dir;
    direction:=dir;
  end;

DESTRUCTOR T_ray.destroy;
  begin end;

FUNCTION T_materialPoint.reflectRayAndReturnRefracted(VAR ray:T_ray):T_ray;
  VAR effectiveRefractionIndex:double;
      newDir:T_Vec3;
  begin
    if normal*ray.direction<0 then begin
      //hit from "outside"
      effectiveRefractionIndex:=1/relRefractionIdx;
    end else begin
      //hit from "inside"
      normal:=-1*normal;
      effectiveRefractionIndex:=relRefractionIdx;
    end;
    if isTransparent then begin
      if system.sqr(effectiveRefractionIndex-1)<1E-3
      then result.createRefracted(position,ray.direction,RAY_STEP_EPSILON)
      else result.createRefracted(position,normed(effectiveRefractionIndex*ray.direction+normal*((effectiveRefractionIndex-1)*(ray.direction*normal))),RAY_STEP_EPSILON);
      if refractDistortion>0 then begin
        repeat
          newDir:=normed(result.direction+refractDistortion*randomVecInUnitSphere);
        until (newDir*normal>0) and (newDir*result.direction>random);
        result.direction:=newDir;
      end;
    end else begin
      result:=ray;
      result.state:=RAY_STATE_REFRACTED;
    end;


    ray.direction:=ray.direction-normal*( 2*(ray.direction*normal));
    ray.start:=position+RAY_STEP_EPSILON*ray.direction;
    ray.state:=ray.state or RAY_STATE_REFLECTED;
  end;

PROCEDURE T_materialPoint.modifyReflectedRay(VAR ray:T_ray);
  VAR newDir:T_Vec3;
  begin
    if reflectDistortion>0 then begin
      repeat
        newDir:=normed(ray.direction+reflectDistortion*randomVecInUnitSphere);
      until (newDir*normal>0) and (newDir*ray.direction>random);
      ray.direction:=newDir;
    end;
  end;

FUNCTION T_materialPoint.getRayForLightScan(CONST rayState:byte):T_ray;
  VAR outDirection:T_Vec3;
  begin
    repeat
      outDirection:=randomVecOnUnitSphere;
      if outDirection*normal<0 then outDirection:=-1*outDirection;
    until outDirection*normal>random;
    result.createWithState(position,outDirection,RAY_STEP_EPSILON,rayState);
  end;

PROCEDURE T_ray.modifyReflected(CONST normal:T_Vec3; CONST reflectDistortion:double);
  VAR newDir:T_Vec3;
  begin
    if reflectDistortion>0 then begin
      repeat
        newDir:=normed(direction+reflectDistortion*randomVecInUnitSphere);
      until (newDir*normal>0) and (newDir*direction>random);
      direction:=newDir;
    end;
  end;

CONSTRUCTOR T_view.create(screenWidth,screenHeight:longint; eye,lookat:T_Vec3; openingAngleInDegrees:double);
  begin
    eyeDistortion:=0;
    xRes:=screenWidth;
    yRes:=screenHeight;
    eyepoint:=eye;
    lookDir:=eye-lookat;
    openingAngleInDegrees:=norm(lookDir)/(tan((90-openingAngleInDegrees)*pi/180)*sqrt(xRes*xRes+yRes*yRes));
    right  :=openingAngleInDegrees*normed(cross(lookDir,newVector(0,1,0))); //naive up-vector is (0,1,0); compute right-vector from normalized cross product
    up     :=openingAngleInDegrees*normed(cross(lookDir,right)); //corrected up-vector is look x right
  end;

PROCEDURE T_view.setLensDistortion(eyeSize:double; sharpAtDistance:double);
  VAR aid:double;
  begin
    if eyeDistortion=0 then begin
      eyeDistortion:=eyeSize;
      aid:=sharpAtDistance/norm(lookDir);
      up     :=up*aid;
      right  :=right*aid;
      lookDir:=lookDir*aid;
    end;
  end;

PROCEDURE T_view.changeResolution(screenWidth,screenHeight:longint);
  VAR aid:double;
  begin
    aid:=sqrt((xRes*xRes+yRes*yRes)/(screenHeight*screenHeight+screenWidth*screenWidth));
    xRes:=screenWidth;
    yRes:=screenHeight;
    right  :=aid*right;
    up     :=aid*up;
  end;


DESTRUCTOR T_view.destroy;
  begin

  end;

FUNCTION T_view.getRay(CONST x,y:double):T_ray;
  VAR d:T_Vec3;
  begin
    if eyeDistortion<=0 then result.createPrimary(eyepoint,normed((x-xRes*0.5)*right+(y-yRes*0.5)*up-lookDir),0)
    else begin
      d:=randomVecInUnitSphere*eyeDistortion;
      result.createPrimary(eyepoint+d,normed((x-xRes*0.5)*right+(y-yRes*0.5)*up-lookDir-d),0);
    end;
  end;

PROCEDURE T_view.getYPlaneHitCoordinates(CONST screenX,screenY,worldY:double; OUT worldX,worldZ:double);
  VAR d:T_Vec3;
      t:double;
  begin
    d:=screenX*right*xRes+screenY*up*yRes-lookDir;
    t:=(worldY-eyepoint[1])/d[1];
    d:=eyepoint+d*t;
    worldX:=d[0];
    worldZ:=d[2];
  end;

CONSTRUCTOR T_node.create(calc:FT_calcNodeCallback; u,v:double);
  VAR h:double;
  begin
    position:=calc(u,v);
    normal:=zeroVec;
    h:=1E-4;
    repeat
      normal:=normed(cross(calc(u+h,v)-position,
                           calc(u,v+h)-position));
      h:=h*2;
    until (sqNorm(normal)>0.999) or (h>1);
  end;

CONSTRUCTOR T_node.create(p,n:T_Vec3);
  begin
    position:=p;
    normal:=n;
  end;

DESTRUCTOR T_node.destroy;
  begin
  end;

PROCEDURE T_Graph.distortEdge(e:longint);
  CONST epsilon=1E-6;
  VAR i,ke,ki:longint;
      adjacent:array[0..1] of record
                 ps:T_Vec3;
                 count:longint;
               end;
      aid:double;
      n:P_node;
  begin
    for i:=0 to 1 do with adjacent[i] do begin
      ps:=zeroVec;
      count:=0;
    end;
    for i:=0 to length(edge)-1 do if (i<>e) then begin
      for ke:=0 to 1 do for ki:=0 to 1 do if edge[e].ni[ke]=edge[i].ni[ki] then with adjacent[ke] do begin
        ps:=ps+node[edge[i].ni[1-ki]].n^.position;
        inc(count);
      end;
    end;
    for i:=0 to 1 do with adjacent[i] do begin
      n:=node[edge[e].ni[i]].n;
      ps:=ps*(1/count)-n^.position;
      aid:=norm(ps);
      if (aid>epsilon) then n^.position:=ps*(epsilon/aid)             +n^.position;
    end;
    for i:=0 to length(edge)-1 do edge[i].len:=edgeLength(i);
  end;

CONSTRUCTOR T_Graph.create(calc:FT_calcNodeCallback; u0,u1:double; uSteps:longint; v0,v1:double; vSteps:longint);
  CONST MIN_VALID_EDGE_LENGTH=1E-12;
  PROCEDURE analyzePeriodicity;
    VAR i:longint;
        diff:double;
    begin
      diff:=0;
      for i:=0 to 100 do diff:=diff+
        sqNorm(surf(u0,v0+i*0.01*(v1-v0))-
               surf(u1,v0+i*0.01*(v1-v0)));
      if (diff<1E-10) then begin
        uPeriod:=abs(u1-u0);
        writeln('u-Periodicity detected');
      end else uPeriod:=-1;

      diff:=0;
      for i:=0 to 100 do diff:=diff+
        sqNorm(surf(u0+i*0.01*(u1-u0),v0)-
               surf(u0+i*0.01*(u1-u0),v1));
      if (diff<1E-10) then begin
        vPeriod:=abs(v1-v0);
        writeln('v-Periodicity detected');
      end else vPeriod:=-1;
    end;


  PROCEDURE flipOptimize;
    VAR i:longint;
        anyFlipped:boolean;
    begin
      repeat
        anyFlipped:=false;
        i:=0;
        while (i<length(edge)) and not(anyFlipped) do begin
          anyFlipped:=anyFlipped or flipEdge(i);
          inc(i);
        end;
      until not(anyFlipped);
    end;

  PROCEDURE distortZeroLengthEdges;
    VAR e:longint;
    begin
      repeat
        e:=0;
        while (e<length(edge)) and (edge[e].len>MIN_VALID_EDGE_LENGTH) do inc(e);
        if e>=length(edge) then exit;
        distortEdge(e);
      until false;
    end;

  FUNCTION addNode(pu,pv:double):longint;
    begin
      result:=length(node);
      setLength(node,result+1);
      with node[result] do begin
        u:=pu;
        v:=pv;
        new(n,create(surf,u,v));
      end;
    end;

  PROCEDURE triangulateQuad(n00,n01,n10,n11,nCtr:longint);
    begin
      addTriangle(n00,n01,nCtr);
      addTriangle(n01,n11,nCtr);
      addTriangle(n11,n10,nCtr);
      addTriangle(n10,n00,nCtr);
      addEdge(n00,nCtr);
      addEdge(n01,nCtr);
      addEdge(n10,nCtr);
      addEdge(n11,nCtr);
      addEdge(n00,n01);
      addEdge(n00,n10);
    end;

  VAR i,j:longint;
  begin
    surf:=calc;
    if uSteps<2 then uSteps:=2;
    if vSteps<2 then vSteps:=2;
    analyzePeriodicity;
    setLength(node,uSteps*vSteps);
    for i:=0 to uSteps-1 do for j:=0 to vSteps-1 do with node[i*vSteps+j] do begin
      if uPeriod<0
        then u:=u0+(u1-u0)*i/(uSteps-1) //nonperiodic in u
        else u:=u0+(u1-u0)*i/uSteps;    //periodic in u
      if vPeriod<0
        then v:=v0+(v1-v0)*j/(vSteps-1) //nonperiodic in v
        else v:=v0+(v1-v0)*j/(vSteps);  //periodic in v
      new(n,create(surf,u,v));
    end;
    if (uPeriod<0) then begin
      if (vPeriod<0) then begin
        for i:=0 to uSteps-2 do for j:=0 to vSteps-2 do begin
          triangulateQuad( i   *vSteps+j,
                           i   *vSteps+j+1,
                          (i+1)*vSteps+j  ,
                          (i+1)*vSteps+j+1,
                          addNode(u0+(u1-u0)*(i+0.5)/(uSteps-1),
                                  v0+(v1-v0)*(j+0.5)/(vSteps-1)));
          if j=vSteps-2 then addEdge( i   *vSteps+j+1,(i+1)*vSteps+j+1);
          if i=uSteps-2 then addEdge((i+1)*vSteps+j  ,(i+1)*vSteps+j+1);
        end;
      end else begin
        for i:=0 to uSteps-2 do for j:=0 to vSteps-1 do begin
          triangulateQuad( i   *vSteps+((j  ) mod vSteps),
                           i   *vSteps+((j+1) mod vSteps),
                          (i+1)*vSteps+((j  ) mod vSteps),
                          (i+1)*vSteps+((j+1) mod vSteps),
                          addNode(u0+(u1-u0)*(i+0.5)/(uSteps-1),
                                  v0+(v1-v0)*(j+0.5)/(vSteps)));
          if i=uSteps-2 then addEdge((i+1)*vSteps+j  ,(i+1)*vSteps+j+1);
        end;
      end;
    end else begin
      if (vPeriod<0) then begin
        for i:=0 to uSteps-1 do for j:=0 to vSteps-2 do begin
          triangulateQuad(((i  ) mod uSteps)*vSteps+j  ,
                          ((i  ) mod uSteps)*vSteps+j+1,
                          ((i+1) mod uSteps)*vSteps+j  ,
                          ((i+1) mod uSteps)*vSteps+j+1,
                          addNode(u0+(u1-u0)*(i+0.5)/(uSteps),
                                  v0+(v1-v0)*(j+0.5)/(vSteps-1)));
          if j=vSteps-2 then addEdge( i   *vSteps+j+1,(i+1)*vSteps+j+1);
        end;
      end else begin
        for i:=0 to uSteps-1 do for j:=0 to vSteps-1 do begin
          triangulateQuad(((i  ) mod uSteps)*vSteps+((j  ) mod vSteps),
                          ((i  ) mod uSteps)*vSteps+((j+1) mod vSteps),
                          ((i+1) mod uSteps)*vSteps+((j  ) mod vSteps),
                          ((i+1) mod uSteps)*vSteps+((j+1) mod vSteps),
                          addNode(u0+(u1-u0)*(i+0.5)/(uSteps),
                                  v0+(v1-v0)*(j+0.5)/(vSteps)));
        end;
      end;
    end;

    //for i:=0 to length(edge)-1 do updateMeetingFaces(edge[i]);
    updateMeetingFaces;
    flipOptimize;
  end;

PROCEDURE T_Graph.optimizedNewNode(CONST node0,node1:T_nodeWithParam; OUT uNew,vNew:double);
  VAR node0pos,node1pos:T_Vec3;
  FUNCTION sampleFidelity(p:T_Vec3):double;
    VAR d0,d1:double;
    begin
      d0:=norm(p-node0pos);
      d1:=norm(p-node1pos);
      result:=-sqr(0.5-d1/(d1+d0));
    end;

  VAR du,dv,u0,u1,v0,v1:double;
      p:T_Vec3;
      d0,d1:double;
      i:longint;

  begin
    node0pos:=node0.n^.position;
    node1pos:=node1.n^.position;

    u0:=node0.u;
    v0:=node0.v;
    u1:=node1.u;
    v1:=node1.v;
    if (uPeriod>0) then begin
      while u1-u0>uPeriod*0.5 do u1:=u1-uPeriod;
      while u0-u1>uPeriod*0.5 do u1:=u1+uPeriod;
    end;
    if (vPeriod>0) then begin
      while v1-v0>vPeriod*0.5 do v1:=v1-vPeriod;
      while v0-v1>vPeriod*0.5 do v1:=v1+vPeriod;
    end;

    du:=(u1-u0)*0.499;
    dv:=(v1-v0)*0.499;
    uNew:=(u0+u1)*0.5;
    vNew:=(v0+v1)*0.5;
    p:=surf(uNew,vNew);
    for i:=0 to 12 do begin
      d0:=sqNorm(p-node0.n^.position);
      d1:=sqNorm(p-node1.n^.position);
      if (d0>d1*1.001) then begin
        uNew:=uNew-du;
        vNew:=vNew-dv;
        p:=surf(uNew,vNew);
      end else if (d1>d0*1.001) then begin
        uNew:=uNew+du;
        vNew:=vNew+dv;
        p:=surf(uNew,vNew);
      end;
      du:=du*0.5;
      dv:=dv*0.5;
    end;
  end;

PROCEDURE T_Graph.splitEdgesLongerThan(threshold:double);
  CONST nullSplit:T_faceDef=(-1,-1,-1);
  VAR faceSplits:array of T_faceDef;

  PROCEDURE splitEdge(edgeIndex:longint);
    FUNCTION adjacentAt(VAR e:T_edgeDef; VAR f:T_faceDef):longint; inline;
      begin
        if (f[0]=e.ni[0]) and (f[1]=e.ni[1]) or
           (f[0]=e.ni[1]) and (f[1]=e.ni[0]) then exit(0);
        if (f[1]=e.ni[0]) and (f[2]=e.ni[1]) or
           (f[1]=e.ni[1]) and (f[2]=e.ni[0]) then exit(1);
        if (f[2]=e.ni[0]) and (f[0]=e.ni[1]) or
           (f[2]=e.ni[1]) and (f[0]=e.ni[0]) then exit(2);
        writeln('ERROR @adjacentAt (',e.ni[0],',',e.ni[1],')/(',f[0],',',f[1],',',f[2],')'); halt;
        result:=-1;
      end;

    VAR i,j,k,ni,adAt:longint;
    begin
      ni:=length(node);
      setLength(node,ni+1);
      with node[ni] do begin
        optimizedNewNode(node[edge[edgeIndex].ni[0]],
                         node[edge[edgeIndex].ni[1]],u,v);
        new(n,create(surf,u,v));
      end;
      for i:=0 to 1 do begin
        k:=edge[edgeIndex].fi[i];
        if (k>=0) then begin
          if k>=length(faceSplits) then begin
            j:=length(faceSplits);
            setLength(faceSplits,k+1);
            while j<length(faceSplits) do begin
              faceSplits[j]:=nullSplit;
              inc(j);
            end;
          end;
          adAt:=adjacentAt(edge[edgeIndex],face[k]);
          if (faceSplits[k,adAt]>=0) then begin
            writeln('duplicate adjacent-at');
            halt;
          end;
          faceSplits[k,adAt]:=ni;
        end;
      end;
      //old edge: 0--1 -> new edges 0--ni--1
      addEdge(edge[edgeIndex].ni[1],ni); //new edge: ni--1
      edge[edgeIndex].ni[1]:=ni;  //mod edge: 0--ni
      edge[edgeIndex].len:=edgeLength(edgeIndex); //update length of modified edge

    end;

  PROCEDURE splitTriangle(index:longint; newNodes:T_faceDef);
    VAR newNodeCount:byte;
        a,b,c,na,nb,nc:longint;
    begin
      newNodeCount:=0;
      if newNodes[0]>=0 then inc(newNodeCount);
      if newNodes[1]>=0 then inc(newNodeCount);
      if newNodes[2]>=0 then inc(newNodeCount);

      case newNodeCount of
        1: begin
             if newNodes[0]>=0 then begin //AaBC
               a:=face[index,0];
               b:=face[index,1];
               c:=face[index,2];
               na:=newNodes[0];
             end else if newNodes[1]>=0 then begin //ABbC
               a:=face[index,1];
               b:=face[index,2];
               c:=face[index,0];
               na:=newNodes[1];
             end else begin //ABCc
               a:=face[index,2];
               b:=face[index,0];
               c:=face[index,1];
               na:=newNodes[2];
             end;
             //     c
             //    /|\
             //   / | \
             //  /  |  \
             // a---na--b
             addEdge(c,na);
             face[index,0]:=a;
             face[index,1]:=na;
             face[index,2]:=c;
             addTriangle(b,c,na);
           end;
        2: begin
             if newNodes[0]<0 then begin //ABbCc
               a:=face[index,1];
               b:=face[index,2];
               c:=face[index,0];
               na:=newNodes[1];
               nb:=newNodes[2];
             end else if newNodes[1]<0 then begin //AaBCc
               a:=face[index,2];
               b:=face[index,0];
               c:=face[index,1];
               na:=newNodes[2];
               nb:=newNodes[0];
             end else begin //AaBbC
               a:=face[index,0];
               b:=face[index,1];
               c:=face[index,2];
               na:=newNodes[0];
               nb:=newNodes[1];
             end;
             //     a
             //    /|\
             //  na | \
             //  / \|  \
             // b---nb--c
             addEdge(a,nb);
             addEdge(na,nb);
             face[index,0]:=a;
             face[index,1]:=nb;
             face[index,2]:=c;
             addTriangle(a,na,nb);
             addTriangle(b,nb,na);
           end;
        3: begin //AaBbCc
             a:=face[index,0];
             b:=face[index,1];
             c:=face[index,2];
             na:=newNodes[0];
             nb:=newNodes[1];
             nc:=newNodes[2];
             //     a
             //    / \
             //  na---nc
             //  / \ / \
             // b---nb--c
             addEdge(na,nc);
             addEdge(na,nb);
             addEdge(nb,nc);
             face[index,0]:=a;
             face[index,1]:=na;
             face[index,2]:=nc;
             addTriangle(b,nb,na);
             addTriangle(c,nc,nb);
             addTriangle(na,nb,nc);
           end;
      end;

      if (face[index,0]<0) or (face[index,1]<0) or (face[index,2]<0) then begin
        writeln('INVALIDATED TRIANGLE!!!');
        halt;
      end;

    end;

  VAR e,i:longint;

  begin
    //find edges to split and generate nodes:--------------------------------
    setLength(faceSplits,0);
    for e:=0 to length(edge)-1 do if edge[e].len>threshold then splitEdge(e);
    if length(faceSplits)<=0 then exit;
    //--------------------------------:find edges to split and generate nodes
    //split affected triangles:----------------------------------------------
    for i:=0 to length(faceSplits)-1 do splitTriangle(i,faceSplits[i]);
    //----------------------------------------------:split affected triangles
    //flip:---------------------------------------------------
    repeat
      i:=0;
      updateMeetingFaces();
      for e:=0 to length(edge)-1 do if flipEdge(e) then begin
        inc(i);
        updateMeetingFaces();
      end;
    until i=0;
    //---------------------------------------------------:flip
  end;

PROCEDURE T_Graph.splitTryingToObtainFaceCount(faceCount:longint);
  VAR i:longint;
      avgLength,threshold:double;
      nonZeroTrianglesBefore,
      nonZeroTrianglesAfter:longint;
  begin
    nonZeroTrianglesAfter:=trianglesWithNonzeroArea;
    nonZeroTrianglesBefore:=nonZeroTrianglesAfter;
    repeat
      avgLength:=0;
      for i:=0 to length(edge)-1 do avgLength:=avgLength+edge[i].len;
      avgLength:=avgLength/length(edge);
      threshold:=avgLength/sqrt(faceCount/nonZeroTrianglesBefore);

      writeln('splitting (',threshold,') - currently ',i,' faces');
      splitEdgesLongerThan(threshold);
      nonZeroTrianglesAfter:=trianglesWithNonzeroArea;

    until (nonZeroTrianglesAfter>=faceCount) or (nonZeroTrianglesAfter<nonZeroTrianglesBefore+4);
    writeln('aim/result ratio = ',faceCount/nonZeroTrianglesAfter:0:5,' ',faceCount:8,' ',nonZeroTrianglesAfter:8);
  end;

FUNCTION  T_Graph.hasEdge(a,b:longint):longint;
  VAR i:longint;
  begin
    i:=0;
    while (i<length(edge)) and ((edge[i].ni[0]<>a) or (edge[i].ni[1]<>b)) and
                               ((edge[i].ni[0]<>b) or (edge[i].ni[1]<>a)) do inc(i);
    if i<length(edge) then result:=i else result:=-1;
  end;

FUNCTION areAdjacent(VAR e:T_edgeDef; VAR f:T_faceDef):boolean; inline;
  begin
    with e do result:=
      (f[0]=ni[0]) and ((f[1]=ni[1]) or (f[2]=ni[1])) or
      (f[1]=ni[0]) and ((f[2]=ni[1]) or (f[0]=ni[1])) or
      (f[2]=ni[0]) and ((f[0]=ni[1]) or (f[1]=ni[1]));
  end;

PROCEDURE T_Graph.updateMeetingFaces(VAR e:T_edgeDef);
  VAR i:longint;
  begin
    with e do begin
      //mark both result entries as "unset"
      fi[0]:=-1;
      fi[1]:=-1;
      i:=0;
      while (i<length(face)) and (fi[1]=-1) do begin //check all faces...
        if areAdjacent(e,face[i]) then begin
          if fi[0]=-1 then fi[0]:=i
                      else fi[1]:=i;
        end;
        inc(i);
      end;
    end;
  end;

PROCEDURE T_Graph.updateMeetingFaces();
  VAR fatn:array of array of longint;

  PROCEDURE addFaceAtNode(CONST nodeIndex:longint; CONST faceIndex:longint);
    VAR k:longint;
    begin
      k:=length(fatn[nodeIndex]);
      setLength(fatn[nodeIndex],k+1);
      fatn[nodeIndex,k]:=faceIndex;
    end;

  PROCEDURE findFacesForEdge(VAR e:T_edgeDef);
    VAR i,j:longint;
    begin
      e.fi[0]:=-1;
      e.fi[1]:=-1;
      //pick either node to filter triangles;
      i:=e.ni[0];
      for j:=0 to length(fatn[i])-1 do if areAdjacent(e,face[fatn[i][j]]) then begin
        if   e.fi[0]=-1
        then e.fi[0]:=fatn[i][j]
        else if   e.fi[0]<>fatn[i][j]
             then e.fi[1]:=fatn[i][j];
      end;
    end;

  VAR i,j:longint;
  begin
    setLength(fatn,length(node));
    for i:=0 to length(fatn)-1 do setLength(fatn[i],0);
    for i:=0 to length(face)-1 do for j:=0 to 2 do addFaceAtNode(face[i,j],i);
    for i:=0 to length(edge)-1 do findFacesForEdge(edge[i]);
    for i:=0 to length(fatn)-1 do setLength(fatn[i],0);
    setLength(fatn,0);
  end;

FUNCTION T_Graph.faceOrientation(CONST a,b,c:T_nodeWithParam):double;
  VAR vb,vc:T_Vec3;
  begin
    vb:=newVector(b.u-a.u,b.v-a.v,0);
    vc:=newVector(c.u-a.u,c.v-a.v,0);
    if uPeriod>0 then begin
      while vb[0]> 0.5*uPeriod do vb[0]:=vb[0]-uPeriod;
      while vb[0]<-0.5*uPeriod do vb[0]:=vb[0]+uPeriod;
      while vc[0]> 0.5*uPeriod do vc[0]:=vc[0]-uPeriod;
      while vc[0]<-0.5*uPeriod do vc[0]:=vc[0]+uPeriod;
    end;
    if vPeriod>0 then begin
      while vb[1]> 0.5*vPeriod do vb[1]:=vb[1]-vPeriod;
      while vb[1]<-0.5*vPeriod do vb[1]:=vb[1]+vPeriod;
      while vc[1]> 0.5*vPeriod do vc[1]:=vc[1]-vPeriod;
      while vc[1]<-0.5*vPeriod do vc[1]:=vc[1]+vPeriod;
    end;
    result:=cross(vb,vc)[2];
  end;

PROCEDURE T_Graph.orientFaces;
  VAR i,tmp:longint;
  begin
    for i:=0 to length(face)-1 do if faceOrientation(node[face[i,0]],node[face[i,1]],node[face[i,2]])<0 then begin
      tmp      :=face[i,0];
      face[i,0]:=face[i,1];
      face[i,1]:=tmp;
    end;

  end;

FUNCTION T_Graph.edgeLength(ni0,ni1:longint):double;
  VAR d:T_Vec3;
  begin
    d:=node[ni1].n^.position-node[ni0].n^.position;
    result:=sqrt(d[0]*d[0]+d[1]*d[1]+d[2]*d[2]);
  end;

FUNCTION T_Graph.edgeLength(idx:longint):double;
  VAR d:T_Vec3;
  begin
    d:=node[edge[idx].ni[1]].n^.position-node[edge[idx].ni[0]].n^.position;
    result:=sqrt(d[0]*d[0]+d[1]*d[1]+d[2]*d[2]);
  end;

FUNCTION T_Graph.flipEdge(index:longint):boolean;
//     /b\         /b\
//    / | \       /   \
//   /  |  \     /  0  \
//   a 0|1 c ==> a-----c
//   \  |  /     \  1  /
//    \ | /       \   /
//     \d/         \d/

  FUNCTION paramDist(i0,i1:longint):double;
    VAR du,dv:double;
    begin
      du:=node[i0].u-node[i1].u;
      if uPeriod>0 then begin
        while du> 0.5*uPeriod do du:=du-uPeriod;
        while du<-0.5*uPeriod do du:=du+uPeriod;
      end;
      dv:=node[i0].v-node[i1].v;
      if vPeriod>0 then begin
        while dv> 0.5*vPeriod do dv:=dv-vPeriod;
        while dv<-0.5*vPeriod do dv:=dv+vPeriod;
      end;
      result:=du*du+dv*dv;
    end;

  VAR j,k,                       //plain counters
      ia,ib,ic,id:longint;       //indexes of the involved nodes
      face0idx,face1idx:longint; //indexes of involved faces
  begin
    //face indexes from edge:----//
    face0idx:=edge[index].fi[0]; //
    face1idx:=edge[index].fi[1]; //
    //------:face indexes from edge
    if (face1idx<0) or (edge[index].len<=0) then exit(false);
    result:=false;

    //for both faces: find node, NOT contributing of the common edge:-----------------------------------//
    j:=0; while (face[face0idx,j]=edge[index].ni[0]) or (face[face0idx,j]=edge[index].ni[1]) do inc(j); //
    k:=0; while (face[face1idx,k]=edge[index].ni[0]) or (face[face1idx,k]=edge[index].ni[1]) do inc(k); //
    //-------------------------------------:for both faces: find node, NOT contributing of the common edge
    //translate internal indexes to node indexes:-//
    ia:=face[face0idx, j         ];               //
    ib:=face[face0idx,(j+1) mod 3];               //
    id:=face[face0idx,(j+2) mod 3];               //
    ic:=face[face1idx, k         ];               //
    //---:translate internal indexes to node indexes
    //Now we have the following setup:
    //     /b\
    //    / | \
    //   /  |  \
    //   a 0|1 c
    //   \  |  /
    //    \ | /   Since both faces are oriented the same way,
    //     \d/    we have a triangles (a,b,d) and (c,d,b)
    if (sqNorm(node[ib].n^.position-node[id].n^.position)>
        sqNorm(node[ia].n^.position-node[ic].n^.position)) //fipping replaces longer edge by shorter edge
       and (hasEdge(ia,ic)<0) //the local structure is not tetrahedral!
       and (paramDist(ib,id)>0.25*paramDist(ia,ic))
       //and (paramDistRatio(ib,id,ia,ic)>0.5) //limited increase in parameter-dist
       then begin
      //change faces:
      face[face0idx,(j+2) mod 3]:=ic;
      face[face1idx,(k+2) mod 3]:=ia;
      //Now we have the following setup:
      //      /b\
      //     /   \
      //    /  0  \
      //    a-----c
      //    \  1  /
      //     \   /
      //      \d/
      //flip edge:--------------//
      with edge[index] do begin //
        if ia<ic then begin     //
          ni[0]:=ia;            //
          ni[1]:=ic;            //
        end else begin          //
          ni[0]:=ic;            //
          ni[1]:=ia;            //
        end;                    //
        len:=edgeLength(ia,ic); //
        //writeln('distorting flipped edge');
        //if len<1E-12 then distortEdge(index);
      end;                      //
      //----------------:flip edge
      //update outer edges:----------------------------------------//
      k:=hasEdge(ib,ic); if k>=0 then updateMeetingFaces(edge[k]); //
      k:=hasEdge(ia,id); if k>=0 then updateMeetingFaces(edge[k]); //
      //------------------------------------------:update outer edges
      result:=true;
      //writeln('flipped ',index);
    end;
  end;



FUNCTION T_Graph.triangleArea(index:longint):double;
  VAR a:T_Vec3;
  begin
    if (index<0) or (index>=length(face)) then exit(0);
    a:=                node[face[index,0]].n^.position;
    result:=norm(cross(node[face[index,1]].n^.position-a,
                       node[face[index,2]].n^.position-a));
  end;

PROCEDURE T_Graph.addTriangle(i0, i1, i2: longint);
  begin
    if (i0<0) or (i1<0) or (i2<0) then begin
      writeln('TRYING TO ADD INVALID TRIANGLE!!!');
      halt;
    end;

    setLength(face, length(face) + 1);
    face[length(face) - 1][0] := i0;
    face[length(face) - 1][1] := i1;
    face[length(face) - 1][2] := i2;
  end;

FUNCTION T_Graph.addEdge(node0,node1   : longint):longint;
  begin
    result:=length(edge);
    setLength(edge,result+1);
    with edge[result] do begin
      if node1>node0 then begin ni[0]:=node0; ni[1]:=node1; end
                     else begin ni[0]:=node1; ni[1]:=node0; end;
      len:=edgeLength(node0,node1);
      fi[0]:=-1;
      fi[1]:=-1;
    end;
  end;

FUNCTION T_Graph.addEdge(node0,node1,face0,face1: longint):longint;
  begin
    result:=length(edge);
    setLength(edge,result+1);
    with edge[result] do begin
      if node1>node0 then begin ni[0]:=node0; ni[1]:=node1; end
                     else begin ni[0]:=node1; ni[1]:=node0; end;
      if face1>face0 then begin fi[0]:=face0; fi[1]:=face1; end
                     else begin fi[0]:=face1; fi[1]:=face0; end;
      len:=edgeLength(node0,node1);
    end;
  end;

PROCEDURE T_Graph.writeReport;
  VAR i,k,j:longint;
  begin
    writeln('graph has ',length(node):5,' nodes');
    k:=0;  j:=0;
    for i:=0 to length(edge)-1 do begin
      if edgeLength(i)<1E-50 then inc(k);
      if edgeLength(i)<>edge[i].len then inc(j);
    end;
    writeln('          ',length(edge):5,' edges (',k,' vanishing; ',j,' with wrong length)');
    k:=0; for i:=0 to length(face)-1 do if triangleArea(i)<1E-50 then inc(k);
    writeln('          ',length(face):5,' faces (',k,' vanishing)');
  end;

PROCEDURE T_Graph.writeShortReport;
  begin
    writeln(length(node):8,'N ',length(edge):8,'E ',length(face):8,'F');
  end;

FUNCTION T_Graph.trianglesWithNonzeroArea:longint;
  VAR i:longint;
  begin
    result:=0;
    for i:=0 to length(face)-1 do if triangleArea(i)>0 then inc(result);
  end;

DESTRUCTOR T_Graph.destroy;
begin
  setLength(node, 0);
  setLength(edge, 0);
  setLength(face, 0);
end;

CONSTRUCTOR T_dynamicDarts.create;
  begin
    setLength(dart,0);
    acceptanceRadius:=1;
  end;

DESTRUCTOR T_dynamicDarts.destroy;
  begin
    setLength(dart,0);
  end;

FUNCTION T_dynamicDarts.current:T_Vec3;
  begin
    result:=dart[length(dart)-1];
  end;

FUNCTION T_dynamicDarts.next:T_Vec3;
  VAR i:longint;
  begin
    repeat
      result:=randomVecOnUnitSphere;
      i:=0;
      while (i<length(dart)) and (sqNorm(dart[i]-result)>acceptanceRadius) and (sqNorm(dart[i]+result)>acceptanceRadius) do inc(i);
      if i<length(dart) then acceptanceRadius:=acceptanceRadius*0.99;
    until i>=length(dart);
    setLength(dart,length(dart)+1);
    dart[length(dart)-1]:=result;
  end;

INITIALIZATION
 SetExceptionMask([ exInvalidOp,  exDenormalized,  exZeroDivide,  exOverflow,  exUnderflow,  exPrecision]);

end.
