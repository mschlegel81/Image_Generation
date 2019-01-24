{$MAXSTACKSIZE 100000000}
UNIT raytrace;
INTERFACE
USES cmdLineParseUtil,mypics,math,linAlg3d,sysutils,darts,myGenerics,picChunks,myColors,myStringUtil;
{define debugLightingComponents}

CONST
  integ:array[-1..15] of longint=(-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);

  LIGHTING_MODEL_NOAMB             =0;
  LIGHTING_MODEL_SIMPLE            =1;
  LIGHTING_MODEL_LAZY_PATH_TRACING =2;
  LIGHTING_MODEL_PATH_TRACING      =3;

VAR maxObjectsPerOctreeNode:longint=16;
    handDownThreshold:longint=2;
TYPE
  T_side=(Left,Right,both);
  T_removeObjectLevel=(rml_none,rml_removeOffscreen,rml_retainOnlyDirectlyVisible);
  P_material=^T_material;
  P_traceableObject=^I_traceableObject;

  FT_getLightCallback=FUNCTION():T_pointLightInstance;
  T_pointLight=object
    pos:T_Vec3;
    distort:double;
    col:T_rgbFloatColor;
    infiniteDist:boolean;
    brightnessCap:double;
    func:FT_getLightCallback;

    CONSTRUCTOR create(p:T_Vec3; c:T_rgbFloatColor; d:double; infDist:boolean; cap:double; callback:FT_getLightCallback);
    FUNCTION getInstance(CONST surfaceNormal:T_Vec3):T_pointLightInstance;
    FUNCTION getLookIntoLight        (CONST ray:T_ray; CONST tMax:double; OUT specularMask:byte):T_rgbFloatColor;
    FUNCTION getLookIntoLightIntegral(CONST undistortedRay:T_ray; CONST hitNormal:T_Vec3; CONST distortion:double; CONST tMax:double; VAR specularMask:byte):T_rgbFloatColor;
  end;

  FT_colorOfPosCallback=FUNCTION (normal:T_Vec3):T_rgbFloatColor;
  FT_doubleOfPosCallback=FUNCTION (point:T_Vec3):double;

  T_hitDescription=record
    hitTime:double;
    hitPoint,
    hitNormal:T_Vec3;
    hitObject:P_traceableObject;
  end;

  T_lighting=object
    ambientLight:T_rgbFloatColor;
    pointLight:array of T_pointLight;
    ambientFunc:FT_colorOfPosCallback;
    lightingModel:byte;
    specularLights:boolean;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE addPointLight(position:T_Vec3; color:T_rgbFloatColor);
    PROCEDURE addPointLight(position:T_Vec3; distortion:double; color:T_rgbFloatColor);
    PROCEDURE addPositionedLight(position:T_Vec3; color:T_rgbFloatColor);
    PROCEDURE addPositionedLight(position:T_Vec3; distortion:double; color:T_rgbFloatColor);
    PROCEDURE addPositionedLight(position:T_Vec3; distortion:double; color:T_rgbFloatColor; brightnessCap:double);
    PROCEDURE addLight(lightDef:T_pointLight);
    FUNCTION getInstance(CONST index:longint; CONST surfaceNormal:T_Vec3):T_pointLightInstance;
    FUNCTION getBackground(CONST dir:T_Vec3):T_rgbFloatColor;
    FUNCTION getLookIntoLight(CONST ray:T_ray; CONST tMax:double):T_rgbFloatColor;
  end;

  T_material=object
    diffuseColor  :T_rgbFloatColor;
    diffuseFunc   :FT_colorOfPosCallback;

    reflectiveness:T_rgbFloatColor;
    reflectFunc   :FT_colorOfPosCallback;
    reflectDistortion:double;
    reflectDistFunc:FT_doubleOfPosCallback;

    undirectedGlow,
    normalGlow:T_rgbFloatColor;
    undirectedGlowFunc,
    normalGlowFunc:FT_colorOfPosCallback;

    transparency:T_rgbFloatColor;
    transparencyFunc:FT_colorOfPosCallback;
    relRefractionIdx:double;
    refractDistortion:double;

    CONSTRUCTOR create(baseR,baseG,baseB:double);
    CONSTRUCTOR create(c:T_rgbFloatColor);
    DESTRUCTOR destroy;
    FUNCTION getMaterialPoint(CONST position,normal,rayDirection:T_Vec3; CONST hitTime:double):T_materialPoint;
    FUNCTION getTransparencyLevel(CONST position:T_Vec3):single;
  end;

  I_traceableObject=object
    material:P_material;
    CONSTRUCTOR init(mat:P_material);
    DESTRUCTOR destroy; virtual; abstract;
    FUNCTION rayHits(CONST ray:T_ray; CONST maxHitTime:double; OUT hitDescription:T_hitDescription):boolean; virtual; abstract;
    FUNCTION rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean; virtual;
    FUNCTION isContainedInBox(CONST box:T_boundingBox):boolean; virtual; abstract;
    FUNCTION getBoundingBox:T_boundingBox; virtual; abstract;
    FUNCTION getSamplingPoint:T_Vec3; virtual; abstract;
  end;

  P_triangle=^T_triangle;
  T_triangle=object(I_traceableObject)
    node:array[0..2] of P_node;
    CONSTRUCTOR create(a,b,c:P_node; triMaterial:P_material);
    DESTRUCTOR destroy; virtual;
    FUNCTION rayHits(CONST ray:T_ray; CONST maxHitTime:double; OUT hitDescription:T_hitDescription):boolean; virtual;
    FUNCTION rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean; virtual;
    FUNCTION isContainedInBox(CONST box:T_boundingBox):boolean; virtual;
    FUNCTION getBoundingBox:T_boundingBox; virtual;
    FUNCTION getSamplingPoint:T_Vec3; virtual;
  end;

  P_flatTriangle=^T_FlatTriangle;
  T_FlatTriangle=object(I_traceableObject)
    node:array[0..2] of T_Vec3;
    normal:T_Vec3;
    CONSTRUCTOR create(a,b,c:T_Vec3; triMaterial:P_material);
    DESTRUCTOR destroy; virtual;
    FUNCTION rayHits(CONST ray:T_ray; CONST maxHitTime:double; OUT hitDescription:T_hitDescription):boolean; virtual;
    FUNCTION rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean; virtual;
    FUNCTION isContainedInBox(CONST box:T_boundingBox):boolean; virtual;
    FUNCTION getBoundingBox:T_boundingBox; virtual;
    FUNCTION getSamplingPoint:T_Vec3; virtual;
  end;

  P_axisParallelQuad=^T_axisParallelQuad;
  T_axisParallelQuad=object(I_traceableObject)
    qBox:T_boundingBox;
    CONSTRUCTOR create(c1,c2:T_Vec3; mat:P_material);
    DESTRUCTOR destroy; virtual;
    FUNCTION rayHits(CONST ray:T_ray; CONST maxHitTime:double; OUT hitDescription:T_hitDescription):boolean; virtual;
    FUNCTION rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean; virtual;
    FUNCTION isContainedInBox(CONST box:T_boundingBox):boolean; virtual;
    FUNCTION getBoundingBox:T_boundingBox; virtual;
    FUNCTION getSamplingPoint:T_Vec3; virtual;
  end;

  P_sphere=^T_sphere;
  T_sphere=object(I_traceableObject)
    center:T_Vec3;
    radius:double;
    CONSTRUCTOR create(sphereCenter:T_Vec3; sphereRadius:double; mat:P_material);
    DESTRUCTOR destroy; virtual;
    FUNCTION rayHits(CONST ray:T_ray; CONST maxHitTime:double; OUT hitDescription:T_hitDescription):boolean; virtual;
    FUNCTION rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean; virtual;
    FUNCTION isContainedInBox(CONST box:T_boundingBox):boolean; virtual;
    FUNCTION getBoundingBox:T_boundingBox; virtual;
    FUNCTION getSamplingPoint:T_Vec3; virtual;
  end;

  P_kdTree=^T_kdTree;
  T_kdTree=object
    obj:array of P_traceableObject;
    splitDirection:byte;
    splitOffset:double;
    subTrees:array[0..1] of P_kdTree;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE addObject(CONST o:P_traceableObject);
    PROCEDURE removeObject(CONST o:P_traceableObject);
    PROCEDURE refineTree(CONST treeBox:T_boundingBox; CONST aimObjectsPerNode:longint);
    FUNCTION rayHitsObjectInTree(CONST entryTime,exitTime:double; CONST ray:T_ray; OUT hitDescription:T_hitDescription):boolean;
    FUNCTION rayHitsObjectInTreeInaccurate(CONST entryTime,exitTime:double; CONST ray:T_ray; CONST maxHitTime:double):boolean;
  end;

  T_octreeRoot=object
    allObjects:array of P_traceableObject;
    allNodes:array of P_node;
    tree:T_kdTree;

    maxDepth:byte;
    basePlane:record
      present:boolean;
      yPos:double;
      pseudoObject:I_traceableObject;
    end;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE registerNode(node:P_node);
    PROCEDURE addObject(CONST obj:P_traceableObject);
    PROCEDURE removeObject(obj:P_traceableObject; CONST doDispose:boolean);
    PROCEDURE addFlatTriangle(a,b,c:T_Vec3; material:P_material);
    PROCEDURE initialize(calc:FT_calcNodeCallback; u0,u1:double; uSteps:longint; v0,v1:double; vSteps:longint; material:P_material);
    PROCEDURE initialize(calc:FT_calcNodeCallback; VAR view:T_view; avgWorldY:double; steps:longint; material:P_material);
    PROCEDURE initialize(calc:FT_calcNodeCallback; u0,u1,v0,v1:double; nodeCount,fixedUNodes,fixedVNodes:longint; material:P_material);
    PROCEDURE initialize(calc:FT_calcNodeCallback; u0,u1,v0,v1:double; nodeCount:longint; material:P_material);
    PROCEDURE initializeLocRef(calc:FT_calcNodeCallback; u0,u1:double; iu0:longint; v0,v1:double; iv0:longint; splitThreshold:double; material:P_material);
    PROCEDURE initializeFacets(calc:FT_calcNodeCallback; u0,u1:double; iu0:longint; v0,v1:double; iv0:longint; splitThreshold:double; material:P_material);
    FUNCTION rayHitsObjectInTree          (VAR ray:T_ray; OUT hitMaterialPoint:T_materialPoint):boolean;
    FUNCTION rayHitsObjectInTreeInaccurate(VAR ray:T_ray;                    CONST tMax:double):boolean;
    FUNCTION lightVisibility(CONST hitMaterialPoint:T_materialPoint; CONST lazy:boolean; CONST light:T_pointLight; CONST undistortedRay:T_ray; VAR shadowByte:byte):T_rgbFloatColor; inline;
    PROCEDURE getHitColor(CONST pixelX,pixelY:longint; CONST firstRun:boolean; VAR colors:T_structuredHitColor);

    PROCEDURE addBasePlane(y:double; material:P_material);
  end;

PROCEDURE calculateImage(CONST removeObjects:T_removeObjectLevel=rml_none);

VAR
  reflectionDepth:longint=2;
  renderImage
  {$ifdef debugLightingComponents},pathImage,dirImage,restImage{$endif}:T_rawImage;

  view:T_view;
  lighting:T_lighting;
  tree:T_octreeRoot;
  numberOfCPUs:longint=8;
  keepDump:boolean=false;
  globalRenderTolerance:double=1;

IMPLEMENTATION
VAR renderThreadID:array[0..15] of TThreadID;
    chunkToPrepare:array[0..15] of longint;

CONSTRUCTOR T_pointLight.create(p:T_Vec3; c:T_rgbFloatColor; d:double; infDist:boolean; cap:double; callback:FT_getLightCallback);
  begin
    if infDist then pos:=normed(p) else pos:=p;
    distort:=d;
    col:=c;
    infiniteDist:=infDist;
    brightnessCap:=cap;
    func:=callback;
  end;

FUNCTION T_pointLight.getInstance(CONST surfaceNormal:T_Vec3):T_pointLightInstance;
  begin
    if func=nil then begin
      result.create(pos,col,infiniteDist,brightnessCap);
      if distort>1E-10 then begin
        if infiniteDist then begin
          result.pos:=normed(result.pos+distort*randomVecInUnitSphere);
          if distort<(pos*surfaceNormal) then result.pseudoPos:=pos*(1-0.2*distort*distort)
                                         else result.pseudoPos:=pos;
          result.pseudoPos:=pos;
        end else begin
          result.pos:=result.pos+distort*randomVecInUnitSphere;
          result.pseudoPos:=result.pos;
        end;
      end;
    end else begin
      result              :=func();
      result.pseudoPos    :=pos;
      result.infiniteDist :=infiniteDist;
      result.brightnessCap:=brightnessCap;
    end;
  end;

FUNCTION T_pointLight.getLookIntoLight(CONST ray:T_ray; CONST tMax:double; OUT specularMask:byte):T_rgbFloatColor;
  FUNCTION sphereIntersection_finiteDist(CONST center:T_Vec3; CONST distRad:double):double; inline;
    VAR inRoot:double;
        relCenter:T_Vec3;
        dc,radius:double;
        t0,t1:double;
    begin
      if distRad<1E-2 then radius:=1E-2
                      else radius:=distRad;
      relCenter:=center-ray.start; //relative center
      inRoot:=system.sqr(ray.direction*relCenter)+(radius*radius-sqNorm(relCenter));
      if inRoot<=0 then exit(0);
      inRoot:=sqrt(inRoot); //always: >0
      dc:=ray.direction*relCenter;
      t0:=dc-inRoot;
      t1:=dc+inRoot;
      if t0<0 then t0:=0;
      if t1>tMax then t1:=tMax;
      if t1<t0 then begin specularMask:=specularMask or SPECMASK_SHADOW; result:=0; end
               else begin specularMask:=specularMask or SPECMASK_LIGHT;  result:=(t1-t0)/(radius*radius*radius*pi*4/3); end;
    end;

  FUNCTION sphereIntersection_infiniteDist(CONST center:T_Vec3; CONST distRad:double):double; inline;
    VAR inRoot:double;
        relCenter:T_Vec3;
        radius:double;
    begin
      if distRad<1E-2 then radius:=1E-2
                      else radius:=distRad;
      relCenter:=center-ray.direction; //relative center
      inRoot:=(1-sqNorm(relCenter)/(radius*radius));
      if inRoot<=0 then begin specularMask:=specularMask or SPECMASK_SHADOW; result:=0; end
                   else begin specularMask:=specularMask or SPECMASK_LIGHT;  result:=(2*sqrt(inRoot))/(radius*radius*pi*4/3); end;
    end;

  VAR LI:T_pointLightInstance;
      i:longint;
  begin
    if func=nil then begin
      if infiniteDist
      then begin
        if tMax<9E19
          then result:=BLACK //not looking into infinity -> light in infinite distance is not seen
          else result:=col*sphereIntersection_infiniteDist(pos,distort)
      end else result:=col*sphereIntersection_finiteDist  (pos,distort);
    end else begin
      result:=BLACK;
      if infiniteDist then begin
        if tMax<9E19 then result:=BLACK //not looking into infinity -> light in infinite distance is not seen
        else for i:=0 to 9 do begin
          LI:=func();
          result+=LI.col*(0.1*sphereIntersection_infiniteDist(LI.pos,0));
        end;
      end else for i:=0 to 9 do begin
        LI:=func();
        result+=LI.col*(0.1*sphereIntersection_finiteDist  (LI.pos,0));
      end;
    end;
  end;

FUNCTION T_pointLight.getLookIntoLightIntegral(CONST undistortedRay:T_ray; CONST hitNormal:T_Vec3; CONST distortion:double; CONST tMax:double; VAR specularMask:byte):T_rgbFloatColor;
  VAR i,imax:longint;
      ray:T_ray;
      maskAid:byte;
  begin
    result:=BLACK;
    if distortion<=1E-3 then imax:=1 else begin
      if (specularMask and SPECMASK_BOTH  )=SPECMASK_BOTH   then imax:=64 else imax:=8;
      if (specularMask and SHADOWMASK_BOTH)=SHADOWMASK_BOTH then imax:=imax shr 2;
    end;
    for i:=1 to imax do begin
      ray:=undistortedRay;
      ray.modifyReflected(hitNormal,distortion);
      result+=getLookIntoLight(ray,tMax,maskAid);
      specularMask:=specularMask or maskAid;
    end;
    result:=result*(1/imax);
  end;

CONSTRUCTOR T_lighting.create;
  begin
    specularLights:=false;
    setLength(pointLight,0);
    lightingModel:=LIGHTING_MODEL_SIMPLE;
    ambientLight:=BLACK;
    ambientFunc:=nil;
  end;

DESTRUCTOR T_lighting.destroy;
  begin
    setLength(pointLight,0);
  end;

PROCEDURE T_lighting.addPointLight(position:T_Vec3; distortion:double; color:T_rgbFloatColor);
  begin
    setLength(pointLight,length(pointLight)+1);
    pointLight[length(pointLight)-1].create(position,color,distortion,true,1,nil);
  end;

PROCEDURE T_lighting.addPositionedLight(position:T_Vec3; distortion:double; color:T_rgbFloatColor; brightnessCap:double);
  begin
    setLength(pointLight,length(pointLight)+1);
    pointLight[length(pointLight)-1].create(position,color,distortion,false,brightnessCap,nil);
  end;

PROCEDURE T_lighting.addPositionedLight(position:T_Vec3; distortion:double; color:T_rgbFloatColor);
  begin addPositionedLight(position,distortion,color,1E6); end;
PROCEDURE T_lighting.addPointLight(position:T_Vec3; color:T_rgbFloatColor);
  begin addPointLight(position,0,color); end;
PROCEDURE T_lighting.addPositionedLight(position:T_Vec3; color:T_rgbFloatColor);
  begin addPositionedLight(position,0,color); end;

PROCEDURE T_lighting.addLight(lightDef:T_pointLight);
  begin
    setLength(pointLight,length(pointLight)+1);
    pointLight[length(pointLight)-1]:=lightDef;
  end;

FUNCTION T_lighting.getInstance(CONST index:longint; CONST surfaceNormal:T_Vec3):T_pointLightInstance;
  begin
    result:=pointLight[index].getInstance(surfaceNormal);
  end;

FUNCTION T_lighting.getBackground(CONST dir:T_Vec3):T_rgbFloatColor;
  begin
    if ambientFunc=nil then result:=ambientLight
                       else result:=ambientFunc(dir);
  end;

FUNCTION T_lighting.getLookIntoLight(CONST ray:T_ray; CONST tMax:double):T_rgbFloatColor;
  VAR i:longint;
      dummyMask:byte;
  begin
    result:=BLACK;
    if not(specularLights) then for i:=0 to length(pointLight)-1 do begin
      dummyMask:=SPECMASK_NONE;
      result+=pointLight[i].getLookIntoLight(ray,tMax,dummyMask);
    end;
  end;

CONSTRUCTOR T_material.create(baseR,baseG,baseB:double);
  begin
    create(rgbColor(baseR,baseG,baseB));
  end;

CONSTRUCTOR T_material.create(c:T_rgbFloatColor);
  begin
    diffuseColor  :=c;
    diffuseFunc   :=nil;

    reflectiveness:=BLACK;
    reflectFunc   :=nil;
    reflectDistortion:=0;
    reflectDistFunc:=nil;

    normalGlow:=BLACK;
    undirectedGlow:=BLACK;
    normalGlowFunc:=nil;
    undirectedGlowFunc:=nil;

    transparency:=BLACK;
    transparencyFunc:=nil;
    relRefractionIdx:=1.4;
    refractDistortion:=0;
  end;

DESTRUCTOR T_material.destroy;
  begin

  end;

FUNCTION T_material.getMaterialPoint(CONST position,normal,rayDirection:T_Vec3; CONST hitTime:double):T_materialPoint;
  FUNCTION NVL(CONST func:FT_colorOfPosCallback;  CONST pos:T_Vec3; CONST col:T_rgbFloatColor):T_rgbFloatColor; inline; begin if func=nil then result:=col else result:=func(pos); end;
  FUNCTION NVL(CONST func:FT_doubleOfPosCallback; CONST pos:T_Vec3; CONST col:double         ):double;          inline; begin if func=nil then result:=col else result:=func(pos); end;

  begin
    result.create(
      position,
      normal,
      hitTime,
      NVL(diffuseFunc       ,position,diffuseColor),
      NVL(normalGlowFunc    ,position,normalGlow)*abs(rayDirection*normal)+
      NVL(undirectedGlowFunc,position,undirectedGlow   ),
      NVL(transparencyFunc  ,position,transparency     ),
      NVL(reflectFunc       ,position,reflectiveness   ),
      NVL(reflectDistFunc   ,position,reflectDistortion),
      refractDistortion,
      relRefractionIdx);
  end;

FUNCTION T_material.getTransparencyLevel(CONST position:T_Vec3):single;
  begin
    if transparencyFunc=nil then result:=greyLevel(transparency)
                            else result:=greyLevel(transparencyFunc(position));
  end;

CONSTRUCTOR T_kdTree.create;
  begin
    setLength(obj,0);
    splitDirection:=0;
    splitOffset:=0;
    subTrees[0]:=nil;
    subTrees[1]:=nil;
  end;

DESTRUCTOR T_kdTree.destroy;
  begin
    setLength(obj,0);
    splitDirection:=0;
    splitOffset:=0;
    if subTrees[0]<>nil then dispose(subTrees[0],destroy);
    if subTrees[1]<>nil then dispose(subTrees[1],destroy);
  end;

PROCEDURE T_kdTree.addObject(CONST o:P_traceableObject);
  begin
    setLength(obj,length(obj)+1);
    obj[length(obj)-1]:=o;
  end;

PROCEDURE T_kdTree.removeObject(CONST o:P_traceableObject);
  VAR i,j:longint;
  begin
    i:=0;
    while i<length(obj) do
    if obj[i]=o then begin
      for j:=i to length(obj)-2 do obj[j]:=obj[j+1];
      setLength(obj,length(obj)-1);
    end else inc(i);
    for i:=0 to 1 do if subTrees[i]<>nil then subTrees[i]^.removeObject(o);
  end;

PROCEDURE T_kdTree.refineTree(CONST treeBox:T_boundingBox; CONST aimObjectsPerNode:longint);
  TYPE T_side=(Left,Right,both);
  VAR dist:array[0..2] of T_arrayOfDouble;
      i,axis:longint;
      p:T_Vec3=(0,0,0);
      objectInSplitPlaneCount:array[0..2] of longint;
      box:T_boundingBox;
      subBox:array[0..1] of T_boundingBox;
  FUNCTION sideOf(CONST b:T_boundingBox; CONST axis:byte; CONST splitPlane:double):T_side;
    begin
      if b.upper[axis]<splitPlane then exit(Left);
      if b.lower[axis]>splitPlane then exit(Right);
      result:=both;
    end;

  begin
    if (length(obj)<=aimObjectsPerNode) then exit;
    if subTrees[0]=nil then begin
      for axis:=0 to 2 do dist[axis].create;
      //create lists of bounding box midpoints
      for i:=0 to length(obj)-1 do begin
        box:=obj[i]^.getBoundingBox;
        for axis:=0 to 2 do begin
          append(dist[axis],box.lower[axis]);
          append(dist[axis],box.upper[axis]);
        end;
      end;
      //sort lists
      for axis:=0 to 2 do sort(dist[axis]);
      i:=length(obj);
      for axis:=0 to 2 do p[axis]:=dist[axis][i];
      begin
        //determine optimal split direction
        for axis:=0 to 2 do begin
          objectInSplitPlaneCount[axis]:=0;
          for i:=0 to length(obj)-1 do if sideOf(obj[i]^.getBoundingBox,axis,p[axis])=both then inc(objectInSplitPlaneCount[axis]);
        end;
        if (objectInSplitPlaneCount[0]<=objectInSplitPlaneCount[1]) then begin
          if objectInSplitPlaneCount[0]<=objectInSplitPlaneCount[2] then splitDirection:=0 else splitDirection:=2;
        end else begin
          if objectInSplitPlaneCount[1]<=objectInSplitPlaneCount[2] then splitDirection:=1 else splitDirection:=2;
        end;
        splitOffset:=p[splitDirection];
      end;
      i:=length(dist[0])-1;
      for axis:=0 to 2 do setLength(dist[axis],0);
    end else begin
      objectInSplitPlaneCount[splitDirection]:=0;
      for i:=0 to length(obj)-1 do if sideOf(obj[i]^.getBoundingBox,splitDirection,p[splitDirection])=both then inc(objectInSplitPlaneCount[splitDirection]);
    end;
    subBox[0]:=treeBox; subBox[0].upper[splitDirection]:=splitOffset;
    subBox[1]:=treeBox; subBox[1].lower[splitDirection]:=splitOffset;

    //If a pathological case is encountered stop here to avoid infinite recursion
    if objectInSplitPlaneCount[splitDirection]>=length(obj) then exit;
    //perform split
    new(subTrees[0],create);
    new(subTrees[1],create);
    for i:=0 to length(obj)-1 do case sideOf(obj[i]^.getBoundingBox,splitDirection,splitOffset) of
      Left : if obj[i]^.isContainedInBox(subBox[0]) then subTrees[0]^.addObject(obj[i]);
      Right: if obj[i]^.isContainedInBox(subBox[1]) then subTrees[1]^.addObject(obj[i]);
      both : begin
               if obj[i]^.isContainedInBox(subBox[0]) then subTrees[0]^.addObject(obj[i]);
               if obj[i]^.isContainedInBox(subBox[1]) then subTrees[1]^.addObject(obj[i]);
             end;
    end;
    //Another possible pathology:
    if (length(subTrees[0]^.obj)>=length(obj)) or (length(subTrees[1]^.obj)>=length(obj)) then begin
      dispose(subTrees[0],destroy); subTrees[0]:=nil;
      dispose(subTrees[1],destroy); subTrees[1]:=nil;
      exit;
    end;
    //remove objects
    setLength(obj,0);
    //recurse
    subTrees[0]^.refineTree(subBox[0],aimObjectsPerNode);
    subTrees[1]^.refineTree(subBox[1],aimObjectsPerNode);
  end;

FUNCTION T_kdTree.rayHitsObjectInTree(CONST entryTime,exitTime:double; CONST ray:T_ray; OUT hitDescription:T_hitDescription):boolean;
  VAR i:longint;
      rayMoves,rayEnters:shortint;
      planeHitTime:double=0;
      newHit:T_hitDescription;
  begin
    result:=false;
    hitDescription.hitTime:=infinity;
    for i:=0 to length(obj)-1 do if obj[i]^.rayHits(ray,hitDescription.hitTime,newHit) then begin
      result:=true;
      hitDescription:=newHit;
      hitDescription.hitObject:=obj[i];
    end;
    if subTrees[0]<>nil then begin
      if      ray.direction[splitDirection]>0 then rayMoves:= 1
      else if ray.direction[splitDirection]<0 then rayMoves:=-1
      else                                         rayMoves:= 0;
      if ray.start[splitDirection]+ray.direction[splitDirection]*entryTime>splitOffset then begin
        rayEnters:=1;
        if rayMoves=1 then rayMoves:=0; //enter right, and move right: no intersecting plane hit
      end else begin
        rayEnters:=0;
        if rayMoves=-1 then rayMoves:=0; //enter left, and move left: no intersecting plane hit
      end;
      if rayMoves<>0 then begin
        planeHitTime:=(splitOffset-ray.start[splitDirection])*ray.invDir[splitDirection];
        if planeHitTime>exitTime then rayMoves:=0;
      end;
      if rayMoves<>0 then begin
        result:=subTrees[rayEnters]^.rayHitsObjectInTree(entryTime,planeHitTime,ray,hitDescription);
        if result then begin
          if (hitDescription.hitTime<planeHitTime) then exit(true);
          //further hits are only of interest if they occur earlier than the already found hit
          if subTrees[rayEnters+rayMoves]^.rayHitsObjectInTree(planeHitTime,hitDescription.hitTime,ray,newHit) then begin
            if hitDescription.hitTime>newHit.hitTime then
               hitDescription       :=newHit;
            result:=true;
          end;
        end else begin
          if subTrees[rayEnters+rayMoves]^.rayHitsObjectInTree(planeHitTime,exitTime,ray,newHit) then begin
            if hitDescription.hitTime>newHit.hitTime then
               hitDescription       :=newHit;
            result:=true;
          end;

        end;
      end else result:=subTrees[rayEnters]^.rayHitsObjectInTree(entryTime,exitTime,ray,hitDescription);
    end;
  end;

FUNCTION T_kdTree.rayHitsObjectInTreeInaccurate(CONST entryTime,exitTime:double; CONST ray:T_ray; CONST maxHitTime:double):boolean;
  VAR i:longint;
      rayMoves,rayEnters:shortint;
      planeHitTime:double=0;
  begin
    result:=false;
    for i:=0 to length(obj)-1 do if obj[i]^.rayHitsInaccurate(ray,maxHitTime) then exit(true);
    if subTrees[0]<>nil then begin
      if      ray.direction[splitDirection]>0 then rayMoves:= 1
      else if ray.direction[splitDirection]<0 then rayMoves:=-1
      else                                         rayMoves:= 0;
      if ray.start[splitDirection]+ray.direction[splitDirection]*entryTime>splitOffset then begin
        rayEnters:=1;
        if rayMoves=1 then rayMoves:=0; //enter right, and move right: no intersecting plane hit
      end else begin
        rayEnters:=0;
        if rayMoves=-1 then rayMoves:=0; //enter left, and move left: no intersecting plane hit
      end;
      if rayMoves<>0 then begin
        planeHitTime:=(splitOffset-ray.start[splitDirection])*ray.invDir[splitDirection];
        if planeHitTime>exitTime then rayMoves:=0;
      end;
      if rayMoves<>0 then begin
        result:=subTrees[rayEnters         ]^.rayHitsObjectInTreeInaccurate(entryTime   ,planeHitTime,ray,maxHitTime)
             or subTrees[rayEnters+rayMoves]^.rayHitsObjectInTreeInaccurate(planeHitTime,exitTime    ,ray,maxHitTime);
      end else result:=subTrees[rayEnters  ]^.rayHitsObjectInTreeInaccurate(entryTime   ,exitTime    ,ray,maxHitTime);
    end;
  end;

CONSTRUCTOR T_octreeRoot.create;
  begin
    maxDepth:=0;
    tree.create;
    basePlane.present:=false;
  end;

DESTRUCTOR T_octreeRoot.destroy;
  VAR i:longint;
  begin
    tree.destroy;
    for i:=0 to length(allObjects)-1 do dispose(allObjects[i],destroy);
    setLength(allObjects,0);
    for i:=0 to length(allNodes)-1 do dispose(allNodes[i],destroy);
    setLength(allNodes,0);
  end;

PROCEDURE T_octreeRoot.registerNode(node:P_node);
  begin
    setLength(allNodes,length(allNodes)+1);
    allNodes[length(allNodes)-1]:=node;
  end;

PROCEDURE T_octreeRoot.addObject(CONST obj:P_traceableObject);
  begin
    tree.addObject(obj);
    setLength(allObjects,length(allObjects)+1);
    allObjects[length(allObjects)-1]:=obj;
  end;

PROCEDURE T_octreeRoot.removeObject(obj:P_traceableObject; CONST doDispose:boolean);
  VAR i,j:longint;
  begin
    tree.removeObject(obj);
    i:=0;
    while (i<length(allObjects)) do
    if allObjects[i]=obj then begin
      for j:=i to length(allObjects)-2 do allObjects[j]:=allObjects[j+1];
      setLength(allObjects,length(allObjects)-1);
    end else inc(i);
    if doDispose then dispose(obj,destroy);
  end;

PROCEDURE T_octreeRoot.addFlatTriangle(a,b,c:T_Vec3; material:P_material);
  VAR tri:P_flatTriangle;
  begin
    new(tri,create(a,b,c,material));
    addObject(tri);
  end;

PROCEDURE T_octreeRoot.initialize(calc:FT_calcNodeCallback; u0,u1:double; uSteps:longint; v0,v1:double; vSteps:longint; material:P_material);
  VAR L:array of array of record node:P_node; tri:array[0..1] of P_triangle; end;
      iu,iv:longint;
  begin
    if uSteps<2 then uSteps:=2;
    if vSteps<2 then vSteps:=2;
    //initialize nodes:---------------------//
    setLength(L,uSteps);                    //
    for iu:=0 to uSteps-1 do begin          //
      setLength(L[iu],vSteps);              //
      for iv:=0 to vSteps-1 do begin        //
        new(L[iu][iv].node,create(calc,     //
          u0+(u1-u0)*(iu/(uSteps-1)),       //
          v0+(v1-v0)*(iv/(vSteps-1))));     //
        registerNode(L[iu][iv].node);
      end;                                  //
    end;                                    //
    //-----------------------:initialize nodes
    //initialize triangles:----------------------------------//
    for iu:=0 to uSteps-2 do for iv:=0 to vSteps-2 do begin  //
      // (iu,iv+1)--(iu+1,iv+1)                              //
      //    |     \      |                                   //
      //    |      \     |                                   //
      // (iu,iv)----(iu+1,iv)                                //
      new(L[iu][iv].tri[0],create(L[iu  ][iv  ].node,        //
                                  L[iu+1][iv  ].node,        //
                                  L[iu  ][iv+1].node,        //
                                  material));                //
      addObject(L[iu][iv].tri[0]);                           //
      new(L[iu][iv].tri[1],create(L[iu+1][iv+1].node,        //
                                  L[iu  ][iv+1].node,        //
                                  L[iu+1][iv  ].node,        //
                                  material));                //
      addObject(L[iu][iv].tri[1]);                           //
    end;                                                     //
    //------------------------------------:initialize triangles
  end;

PROCEDURE T_octreeRoot.initialize(calc:FT_calcNodeCallback; u0,u1,v0,v1:double; nodeCount,fixedUNodes,fixedVNodes:longint; material:P_material);
  VAR su,sv:array of double;

  PROCEDURE determineAxisResolution;
    VAR i,j:longint;
        di,dj:array[0..100] of double;
        point:array [0..100,0..100] of T_Vec3;
        uCount,vCount:longint;

    FUNCTION getU(ru:double):double;
      VAR k:longint;
      begin
        if fixedUNodes>0 then exit(u0+(u1-u0)*ru);
        k:=0;
        while (k<99) and (di[k+1]<ru) do inc(k);
        result:=u0+(u1-u0)*((k+(ru-di[k])/(di[k+1]-di[k]))*0.01);
      end;

    FUNCTION getV(rv:double):double;
      VAR k:longint;
      begin
        if fixedVNodes>0 then exit(v0+(v1-v0)*rv);
        k:=0;
        while (k<99) and (dj[k+1]<rv) do inc(k);
        result:=v0+(v1-v0)*((k+(rv-dj[k])/(dj[k+1]-dj[k]))*0.01);
      end;

    begin
      for i:=0 to 100 do for j:=0 to 100 do point[i,j]:=calc(u0+(u1-u0)*i*0.01,v0+(v1-v0)*j*0.01);
      for i:=0 to 100 do di[i]:=0;
      for j:=0 to 100 do dj[j]:=0;
      for i:=0 to 100 do for j:=0 to 100 do begin
        if i>0 then di[i]:=di[i]+(1E-6+norm(point[i,j]-point[i-1,j]));
        if j>0 then dj[j]:=dj[j]+(1E-6+norm(point[i,j]-point[i,j-1]));
      end;
      for i:=1 to 100 do di[i]:=di[i]+di[i-1];
      for j:=1 to 100 do dj[j]:=dj[j]+dj[j-1];
      if fixedUNodes>1
        then uCount:=fixedUNodes
        else uCount:=round(sqrt(di[100]/dj[100]*nodeCount));
      if fixedVNodes>1
        then vCount:=fixedVNodes
        else vCount:=round(sqrt(dj[100]/di[100]*nodeCount));
      if uCount*vCount=0 then begin
        uCount:=round(sqrt(nodeCount));
        vCount:=uCount;
      end;
      if nodeCount>5000 then writeln('distributing ',uCount*vCount,' of ',nodeCount,' nodes in a ',uCount,'x',vCount,' grid');
      //normalize:
      for i:=0 to 100 do di[i]:=di[i]/di[100];
      for j:=0 to 100 do dj[j]:=dj[j]/dj[100];
      //fill samples:
      setLength(su,uCount); for i:=0 to uCount-1 do su[i]:=getU(i/(uCount-1));
      setLength(sv,vCount); for j:=0 to vCount-1 do sv[j]:=getV(j/(vCount-1));
    end;

  PROCEDURE makePointsAndGrid;
    VAR i,j,k0:longint;
        p00,p01,p10,p11:P_node;
        tri:P_triangle;
    begin
      if nodeCount>5000 then writeln('creating surface nodes');
      k0:=length(allNodes);
      setLength(allNodes,k0+length(su)*length(sv));
      for i:=0 to length(su)-1 do for j:=0 to length(sv)-1 do new(allNodes[k0+i*length(sv)+j],create(calc,su[i],sv[j]));
      if nodeCount>5000 then writeln('creating surface triangles');
      for i:=0 to length(su)-2 do
      for j:=0 to length(sv)-2 do begin
        p00:=allNodes[k0+(i  )*length(sv)+j  ];
        p01:=allNodes[k0+(i  )*length(sv)+j+1];
        p10:=allNodes[k0+(i+1)*length(sv)+j  ];
        p11:=allNodes[k0+(i+1)*length(sv)+j+1];
        if sqNorm(p00^.position-p11^.position)<sqNorm(p01^.position-p10^.position) then begin
          new(tri,create(p00,p11,p01,material)); addObject(tri);
          new(tri,create(p00,p11,p10,material)); addObject(tri);
        end else begin
          new(tri,create(p01,p10,p00,material)); addObject(tri);
          new(tri,create(p01,p10,p11,material)); addObject(tri);
        end;
      end;
    end;

  begin
    determineAxisResolution;
    makePointsAndGrid;
  end;

PROCEDURE T_octreeRoot.initialize(calc:FT_calcNodeCallback; u0,u1,v0,v1:double; nodeCount:longint; material:P_material);
  begin initialize(calc,u0,u1,v0,v1,nodeCount,-1,-1,material); end;

PROCEDURE T_octreeRoot.initialize(calc:FT_calcNodeCallback; VAR view:T_view; avgWorldY:double; steps:longint; material:P_material);
  VAR L:array of array of record node:P_node; tri:array[0..1] of P_triangle; end;
      iu,iv:longint;
      cx,cy:double;
  begin
    if steps<2 then steps:=2;
    //initialize nodes:---------------------//
    setLength(L,steps);                     //
    for iu:=0 to steps-1 do begin           //
      setLength(L[iu],steps);               //
      for iv:=0 to steps-1 do begin         //
        view.getYPlaneHitCoordinates(       //
          (iu/(steps-1))-0.5,               //
          (iv/(steps-1))-0.5,               //
          avgWorldY,                        //
          cx,cy);                           //
        new(L[iu][iv].node,                 //
          create(calc,cx,cy));              //
        registerNode(L[iu][iv].node);
      end;                                  //
    end;                                    //
    //-----------------------:initialize nodes
    //initialize triangles:----------------------------------//
    for iu:=0 to steps-2 do for iv:=0 to steps-2 do begin    //
      // (iu,iv+1)--(iu+1,iv+1)                              //
      //    |     \      |                                   //
      //    |      \     |                                   //
      // (iu,iv)----(iu+1,iv)                                //
      new(L[iu][iv].tri[0],create(L[iu  ][iv  ].node,        //
                                  L[iu+1][iv  ].node,        //
                                  L[iu  ][iv+1].node,        //
                                  material));                //
      addObject(L[iu][iv].tri[0]);                           //
      new(L[iu][iv].tri[1],create(L[iu+1][iv+1].node,        //
                                  L[iu  ][iv+1].node,        //
                                  L[iu+1][iv  ].node,        //
                                  material));                //
      addObject(L[iu][iv].tri[1]);                           //
    end;                                                     //
    //------------------------------------:initialize triangles
  end;

PROCEDURE T_octreeRoot.initializeLocRef(calc:FT_calcNodeCallback; u0,u1:double; iu0:longint; v0,v1:double; iv0:longint; splitThreshold:double; material:P_material);
  VAR g:T_Graph;
      i,i0:longint;
      tri:P_triangle;
  begin
    g.create(calc,u0,u1,iu0,v0,v1,iv0);
    i:=g.trianglesWithNonzeroArea;
    repeat
      i0:=i;
      g.writeShortReport;
      g.splitEdgesLongerThan(splitThreshold);
      i:=g.trianglesWithNonzeroArea;
    until (i<=i0+4) or (i<i0*1.2);
    g.orientFaces;

    g.writeReport;
    i0:=length(allNodes);
    setLength(allNodes,i0+length(g.node));
    for i:=0 to length(g.node)-1 do allNodes[i+i0]:=g.node[i].n;

    for i:=0 to length(g.face)-1 do if g.triangleArea(i)>0 then begin
      new(tri,create(g.node[g.face[i,0]].n,
                     g.node[g.face[i,1]].n,
                     g.node[g.face[i,2]].n,
                     material));
      addObject(tri);
    end;
    g.destroy;
  end;

PROCEDURE T_octreeRoot.initializeFacets(calc:FT_calcNodeCallback; u0,u1:double; iu0:longint; v0,v1:double; iv0:longint; splitThreshold:double; material:P_material);
  VAR g:T_Graph;
      i,i0:longint;
      //sphere:P_sphere;
      mat:array[0..9] of P_material;
  begin
    for i:=0 to 9 do new(mat[i],create(WHITE*(i+1)*0.1));
    g.create(calc,u0,u1,iu0,v0,v1,iv0);
    i:=g.trianglesWithNonzeroArea;
    repeat
      i0:=i;
      g.writeShortReport;
      g.splitEdgesLongerThan(splitThreshold);
      i:=g.trianglesWithNonzeroArea;
    until (i<=i0+4) or (i<i0*1.2);
    g.orientFaces;
    g.writeReport;
    for i:=0 to length(g.face)-1 do if g.triangleArea(i)>0 then addFlatTriangle(
      g.node[g.face[i,0]].n^.position,
      g.node[g.face[i,1]].n^.position,
      g.node[g.face[i,2]].n^.position,
      material);

    for i:=0 to length(g.node)-1 do dispose(g.node[i].n,destroy);
    g.destroy;
  end;

FUNCTION T_octreeRoot.rayHitsObjectInTree(VAR ray:T_ray; OUT hitMaterialPoint:T_materialPoint):boolean;
  VAR hitDescription:T_hitDescription;

  FUNCTION hitsPlane(CONST hasHit:boolean):boolean; inline;
    VAR planeHitTime:double;
    begin
      result:=false;
      if basePlane.present and (abs(ray.direction[1])>NO_DIV_BY_ZERO_EPSILON) then begin
        planeHitTime:=(basePlane.yPos-ray.start[1])*ray.invDir[1];
        if (planeHitTime>0) and (not(hasHit) or (planeHitTime<hitDescription.hitTime)) then begin
          hitDescription.hitTime :=planeHitTime;
          hitDescription.hitPoint:=ray.start+planeHitTime*ray.direction;
          hitDescription.hitNormal:=newVector(0,-sign(ray.direction[1]),0);
          hitDescription.hitObject:=@basePlane.pseudoObject;
          result:=true;
        end;
      end;
    end;

  begin
    result:=tree.rayHitsObjectInTree(0,infinity,ray,hitDescription);
    if hitsPlane(result) then result:=true;
    if result then hitMaterialPoint:=hitDescription.hitObject^.material^.getMaterialPoint(hitDescription.hitPoint,hitDescription.hitNormal,ray.direction,hitDescription.hitTime);
  end;

FUNCTION T_octreeRoot.rayHitsObjectInTreeInaccurate(VAR ray:T_ray; CONST tMax:double):boolean;
  begin
    if  basePlane.present and ((basePlane.yPos-ray.start[1])*ray.direction[1]>0) then exit(true);
    result:=tree.rayHitsObjectInTreeInaccurate(0,tMax,ray,tMax)
  end;

FUNCTION T_octreeRoot.lightVisibility(CONST hitMaterialPoint:T_materialPoint; CONST lazy:boolean; CONST light:T_pointLight; CONST undistortedRay:T_ray; VAR shadowByte:byte):T_rgbFloatColor; inline;
  VAR sray:T_ray;
      dir:T_Vec3;
      tMax,w:double;
      instance:T_pointLightInstance;
      lightIsVisible:boolean;
  begin
    instance:=light.getInstance(hitMaterialPoint.normal);
    with light do begin
      if infiniteDist then begin
        dir:=instance.pos;
        w:=dir*hitMaterialPoint.normal;
        sray.createLightScan(hitMaterialPoint.position,dir,RAY_STEP_EPSILON,lazy);
        if not((w<=0) or rayHitsObjectInTreeInaccurate(sray,infinity))
          then begin
                 result:=hitMaterialPoint.getColorAtPixel(instance);
                 lightIsVisible:=true;
                 shadowByte:=shadowByte or SHADOWMASK_LIGHT;
               end
          else begin
                 result:=BLACK;
                 lightIsVisible:=false;
                 shadowByte:=shadowByte or SHADOWMASK_SHADOW;
               end;
      end else begin
        dir:=instance.pos-hitMaterialPoint.position;
        tMax:=norm(dir);
        dir:=dir*(1/tMax);
        w:=dir*hitMaterialPoint.normal/(tMax*tMax);
        tMax:=tMax-RAY_STEP_EPSILON;
        sray.createLightScan(hitMaterialPoint.position,dir,RAY_STEP_EPSILON,lazy);
        if not((w<=0) or rayHitsObjectInTreeInaccurate(sray,tMax))
          then begin
                 result:=hitMaterialPoint.getColorAtPixel(instance);
                 lightIsVisible:=true;
                 shadowByte:=shadowByte or SHADOWMASK_LIGHT;
               end
          else begin
                 result:=BLACK;
                 lightIsVisible:=false;
                 shadowByte:=shadowByte or SHADOWMASK_SHADOW;
               end;
      end;
    end;
    if lighting.specularLights then begin
      if lightIsVisible and (hitMaterialPoint.isReflective) then begin
        result+=
          hitMaterialPoint.getReflected(
            light.getLookIntoLightIntegral(
              undistortedRay,
              hitMaterialPoint.normal,
              hitMaterialPoint.getReflectDistortion,
              infinity,
              shadowByte)); //tMax
      end else shadowByte:=shadowByte or SPECMASK_SHADOW;
    end;
  end;

PROCEDURE T_octreeRoot.getHitColor(CONST pixelX,pixelY:longint; CONST firstRun:boolean; VAR colors:T_structuredHitColor);
  FUNCTION getHitColor_(ray:T_ray; CONST depth:byte):T_rgbFloatColor;
    VAR hitMaterialPoint:T_materialPoint;
        refractedRay:T_ray;
        i:longint;
        dummyByte:byte;

    FUNCTION ambientLight:T_rgbFloatColor; inline;
      VAR sray:T_ray;
      begin
        sray:=hitMaterialPoint.getRayForLightScan(RAY_STATE_LAZY_LIGHT_SCAN);
        if (greyLevel(lighting.ambientLight)<0.01) and (lighting.ambientFunc=nil) or rayHitsObjectInTreeInaccurate(sray,infinity)
          then result:=BLACK
          else result:=hitMaterialPoint.getLocalAmbientColor(lighting.getBackground(sray.direction));
      end;

    FUNCTION pathLight:T_rgbFloatColor; inline;
      begin
        if (depth<=0)
        then result:=BLACK
        else result:=hitMaterialPoint.getLocalAmbientColor(getHitColor_(hitMaterialPoint.getRayForLightScan(RAY_STATE_PATH_TRACING),depth-1));
      end;

    begin
      if not(rayHitsObjectInTree(ray,hitMaterialPoint)) then begin
        result:=lighting.getBackground(ray.direction);

        if (ray.state and RAY_STATE_PATH_TRACING=0)
          then result+=lighting.getLookIntoLight(ray,infinity);
      end else begin

        if (ray.state and RAY_STATE_PATH_TRACING =0)
          then result:=hitMaterialPoint.localGlowColor+lighting.getLookIntoLight(ray,hitMaterialPoint.hitTime)
          else result:=hitMaterialPoint.localGlowColor;
        refractedRay:=hitMaterialPoint.reflectRayAndReturnRefracted(ray);

        case lighting.lightingModel of
          LIGHTING_MODEL_NOAMB: result+=hitMaterialPoint.getLocalAmbientColor(lighting.getBackground(ray.direction));
          LIGHTING_MODEL_SIMPLE,
          LIGHTING_MODEL_LAZY_PATH_TRACING: result+=ambientLight;
          LIGHTING_MODEL_PATH_TRACING     : result+=pathLight;
        end;

        for i:=0 to length(lighting.pointLight)-1 do begin
          dummyByte:=SHADOWMASK_NONE;
          result+=lightVisibility(hitMaterialPoint,
            lighting.lightingModel in [LIGHTING_MODEL_NOAMB,LIGHTING_MODEL_SIMPLE, LIGHTING_MODEL_LAZY_PATH_TRACING],
            lighting.pointLight[i],
            ray,
            dummyByte);
        end;

        if (depth>0) and (hitMaterialPoint.isReflective) then begin
          hitMaterialPoint.modifyReflectedRay(ray);
          result+=hitMaterialPoint.getReflected(getHitColor_(ray,depth-1));
        end;
        if hitMaterialPoint.isTransparent then begin
          result+=hitMaterialPoint.getRefracted(getHitColor_(refractedRay,depth));
        end;
      end;
    end;

  VAR ray,refractedRay:T_ray;
      sampleIndex:longint;
      maxSampleIndex:longint=2085;
      minSampleIndex:longint=-1;
      hitMaterialPoint:T_materialPoint;

  PROCEDURE correctSampleRangeByMask(VAR mask:byte);
    begin
      if firstRun then begin
        minSampleIndex:=0;
        maxSampleIndex:=1;
        mask:=0;
      end else if mask=1 then begin
        minSampleIndex:=1;
        maxSampleIndex:=2;
        mask:=2;
        maxSampleIndex:=2*maxSampleIndex;
      end else if odd(mask) then begin
        minSampleIndex:=mask-1;
        maxSampleIndex:=minSampleIndex+2;
        if maxSampleIndex>254 then maxSampleIndex:=254;
        mask:=maxSampleIndex;
        minSampleIndex:=2*minSampleIndex;
        maxSampleIndex:=2*maxSampleIndex;
      end else begin
        minSampleIndex:=0;
        maxSampleIndex:=0;
      end;
    end;

  PROCEDURE calculateDirectLight; inline;
    VAR i,j:longint;
    begin
      for i:=0 to length(lighting.pointLight)-1 do with colors.direct[i] do begin
        if shadowByte=SHADOWMASK_BOTH
          then j:=4
          else j:=1;
        while sampleCount<(sampleIndex+1)*j do begin
          col:=col+lightVisibility(
            hitMaterialPoint,
            lighting.lightingModel in [LIGHTING_MODEL_NOAMB,LIGHTING_MODEL_SIMPLE],
            lighting.pointLight[i],
            ray,
            shadowByte);
          inc(sampleCount);
        end;
      end;
    end;

  PROCEDURE addNoHitDirectAndAmbientLight; inline;
    VAR i:longint;
    begin
      for i:=0 to length(lighting.pointLight)-1 do with colors.direct[i] do begin
        if shadowByte=SHADOWMASK_BOTH
          then sampleCount:=(sampleIndex+1)*4
          else sampleCount:=(sampleIndex+1)*1;
      end;
      if (lighting.ambientFunc=nil) and (greyLevel(lighting.ambientLight)<1E-2)
      then                                   colors.pathOrAmbient.weight:=1
      else if colors.pathOrAmbient.scan then colors.pathOrAmbient.weight:=2*(sampleIndex+1);
    end;

  PROCEDURE calculateAmbientLight; inline;
    VAR sray:T_ray;
    begin
      if (lighting.ambientFunc=nil) and (greyLevel(lighting.ambientLight)<1E-2) then begin
        colors.pathOrAmbient.weight:=1;
      end else if colors.pathOrAmbient.scan then while colors.pathOrAmbient.weight<2*(sampleIndex+1) do begin
        sray:=hitMaterialPoint.getRayForLightScan(RAY_STATE_LAZY_LIGHT_SCAN);
        if not(rayHitsObjectInTreeInaccurate(sray,infinity))
          then colors.pathOrAmbient.col:=colors.pathOrAmbient.col+hitMaterialPoint.getLocalAmbientColor(lighting.getBackground(sray.direction));
        colors.pathOrAmbient.weight:=colors.pathOrAmbient.weight+1;
      end;
    end;

  PROCEDURE calculatePathLight; inline;
    VAR sray:T_ray;
    begin
      if colors.pathOrAmbient.scan and (reflectionDepth>0) then while colors.pathOrAmbient.weight<2*(sampleIndex+1) do begin
        sray:=hitMaterialPoint.getRayForLightScan(RAY_STATE_PATH_TRACING);
        colors.pathOrAmbient.col+=hitMaterialPoint.getLocalAmbientColor(getHitColor_(sray,reflectionDepth-1));
        colors.pathOrAmbient.weight+=1;
      end;
    end;

  begin
    correctSampleRangeByMask(colors.antialiasingMask);

    for sampleIndex:=minSampleIndex to maxSampleIndex-1 do begin
      ray:=view.getRay(pixelX+darts_delta[sampleIndex mod 508,0],
                       pixelY+darts_delta[sampleIndex mod 508,1]);
      if rayHitsObjectInTree(ray,hitMaterialPoint) then begin
        colors.rest:=colors.rest+lighting.getLookIntoLight(ray,hitMaterialPoint.hitTime)
                                +hitMaterialPoint.localGlowColor;
        refractedRay:=hitMaterialPoint.reflectRayAndReturnRefracted(ray);
        calculateDirectLight;
        case lighting.lightingModel of
          LIGHTING_MODEL_NOAMB: begin
            colors.pathOrAmbient.weight:=1;
            colors.pathOrAmbient.col:=hitMaterialPoint.getLocalAmbientColor(lighting.getBackground(ray.direction));
          end;
          LIGHTING_MODEL_SIMPLE         : calculateAmbientLight;
          LIGHTING_MODEL_LAZY_PATH_TRACING,
          LIGHTING_MODEL_PATH_TRACING   : calculatePathLight;
        end;

        if (reflectionDepth>0) and (hitMaterialPoint.isReflective) then begin
          hitMaterialPoint.modifyReflectedRay(ray);
          colors.rest:=colors.rest+hitMaterialPoint.getReflected(
            getHitColor_(ray         ,reflectionDepth-1));
        end;
        if (hitMaterialPoint.isTransparent) then begin
          colors.rest:=colors.rest+
          hitMaterialPoint.getRefracted(
            getHitColor_(refractedRay,reflectionDepth  ));
        end;
      end else begin
        colors.rest:=colors.rest+lighting.getBackground(ray.direction)+lighting.getLookIntoLight(ray,1E20);
        addNoHitDirectAndAmbientLight;
      end;
    end;
  end;

PROCEDURE T_octreeRoot.addBasePlane(y:double; material:P_material);
  begin
    basePlane.present:=true;
    basePlane.yPos:=y;
    basePlane.pseudoObject.init(material);
  end;

VAR samplingStatistics,
    currentSamplingStatistics:T_samplingStatistics;

FUNCTION prepareChunk(p:pointer):ptrint;
  VAR chunk:T_colChunk;
      i,j:longint;
  begin
    chunk.create;
    chunk.initForChunk(renderImage.dimensions.width,
                       renderImage.dimensions.height,
                       plongint(p)^,length(lighting.pointLight));
    for i:=0 to chunk.width-1 do for j:=0 to chunk.height-1 do
      tree.getHitColor(chunk.getPicX(i),chunk.getPicY(j),true,chunk.col[i,j]);
    if (globalRenderTolerance>1E-3) then chunk.diffuseShadowMasks;
    while (globalRenderTolerance>1E-3) and chunk.markAlias(globalRenderTolerance) do
      for i:=0 to chunk.width-1 do for j:=0 to chunk.height-1 do if odd(chunk.col[i,j].antialiasingMask) then
        tree.getHitColor(chunk.getPicX(i),chunk.getPicY(j),false,chunk.col[i,j]);

    mergeSamplingStatistics(samplingStatistics       ,chunk.getSamplingStatistics);
    mergeSamplingStatistics(currentSamplingStatistics,chunk.getSamplingStatistics);

    chunk.copyTo(renderImage);
    {$ifdef debugLightingComponents}
    chunk.copyTo_amb   (pathImage);
    chunk.copyTo_direct(dirImage );
    chunk.copyTo_rest  (restImage);
    {$endif}
    chunk.destroy;
    result:=0;
  end;

PROCEDURE calculateImage(CONST xRes,yRes:longint; CONST repairMode:boolean; CONST fileName:string; CONST removeObjects:T_removeObjectLevel);
  TYPE T_arrayOfTracable=array of P_traceableObject;
  CONST tenMinutes=10/(24*60);
  VAR timeOfLastDump:double;
      timeOfLastProgressOutput:double;
      lastProgressOutput:double;
      progTime:array[0..31] of record t,p:double; end;
      startOfCalculation:double;

  FUNCTION dumpName(CONST infix:string='dump'):string; begin result:=ChangeFileExt(paramStr(0),'.'+infix+'.vraw'); end;

  PROCEDURE initProgress(CONST initialProg:double);
    VAR i:longint;
    begin
      startOfCalculation:=now;
      timeOfLastDump:=now;
      timeOfLastProgressOutput:=now;
      lastProgressOutput:=0;
      for i:=0 to 31 do begin progTime[i].p:=initialProg; progTime[i].t:=now; end;
    end;

  PROCEDURE stepProgress(prog:double);
    VAR i:longint;
        total:double;
        remaining:double;
        dumped:string[5]='     ';
    begin
      for i:=0 to 30 do progTime[i]:=progTime[i+1];
      with progTime[31] do begin
        t:=now;
        p:=prog;
      end;

      if now-timeOfLastDump>tenMinutes then begin
        renderImage.saveToFile(dumpName);
        timeOfLastDump:=now;
        dumped:=' DUMP';
      end;

      if ((now-timeOfLastProgressOutput)*24*60*60>5) or (dumped<>'     ') or (prog>=lastProgressOutput+0.1) then begin
        timeOfLastProgressOutput:=now;
        lastProgressOutput:=prog;
        total:=(progTime[31].t-progTime[0].t)/(progTime[31].p-progTime[0].p);
        remaining:=(1-prog)*total;
        writeln(100*prog:5:2,'% total: ',myTimeToStr(now-startOfCalculation+remaining),
                                ' rem: ',myTimeToStr(remaining),
                            ' ready @: ',copy(timeToStr(now+remaining),1,5),dumped,' curr:',string(currentSamplingStatistics),'; avg:',string(samplingStatistics));
        currentSamplingStatistics:=zeroSamplingStatistics;
      end;
    end;

  PROCEDURE initKdTree;
    VAR box:T_boundingBox;
    begin
      box.createInfinite;
      startOfCalculation:=now;
      write('Filling k-d-tree...');
      tree.tree.refineTree(box,14);
      writeln(' done in ',myTimeToStr(now-startOfCalculation));
    end;

  FUNCTION clearTree:T_arrayOfTracable;
    VAR i:longint;
    begin
      setLength(result,length(tree.allObjects));
      for i:=0 to length(result)-1 do result[i]:=tree.allObjects[i];
      tree.tree.destroy;
      tree.tree.create;
      setLength(tree.allObjects,0);
    end;

  PROCEDURE removeOffScrenObjects;
    VAR o:T_arrayOfTracable;
        i:longint;
    begin
      startOfCalculation:=now;
      write('Filtering off-screen objects...');
      o:=clearTree;
      for i:=0 to length(o)-1 do begin
        if view.isOnScreen(o[i]^.getBoundingBox)
        then tree.addObject(o[i])
        else dispose(o[i],destroy);
      end;
      setLength(o,0);
      writeln(' done in ',myTimeToStr(now-startOfCalculation));
      writeln('Reduced geometry: ',length(tree.allObjects),' primitives');
    end;

  PROCEDURE retainOnlyDirectlyVisible;
    FUNCTION canSeeObject(CONST ob:P_traceableObject):boolean;
      VAR ray:T_ray;
          i:longint;
          hitDescription:T_hitDescription;
      begin
        for i:=0 to 3 do begin
          ray.createHidingScan(view.eyepoint,ob^.getSamplingPoint);
          if not(tree.tree.rayHitsObjectInTree(0,infinity,ray,hitDescription)) or (hitDescription.hitObject=ob) then exit(true);
        end;
        result:=false;
      end;

    PROCEDURE reInitTree(CONST toRemove:T_arrayOfTracable);
      VAR o:T_arrayOfTracable;
          i,j:longint;
      begin
        j:=0;
        o:=clearTree;
        for i:=0 to length(o)-1 do begin
          if (j<length(toRemove)) and (toRemove[j]=o[i])
          then inc(j)
          else tree.addObject(o[i]);
        end;
        initKdTree;
      end;

    VAR i:longint;
        retainCount:longint=0;
        totalRemoveCount:longint=0;
        toRemove:T_arrayOfTracable;
    begin
      startOfCalculation:=now;
      writeln('Removing objects not directly visible...');
      i:=0;
      setLength(toRemove,0);
      while (i<length(tree.allObjects)) do begin
        if canSeeObject(tree.allObjects[i])
        then inc(retainCount)
        else begin
          setLength(toRemove,length(toRemove)+1);
          toRemove[length(toRemove)-1]:=tree.allObjects[i];
          inc(totalRemoveCount);
        end;
        inc(i);
        if (i and 1023=0) then writeln('  removed: ',totalRemoveCount:8,'; retained: ',retainCount:8);
        if length(toRemove)>=0.2*length(tree.allObjects) then begin
          reInitTree(toRemove);
          setLength(toRemove,0);
          i:=retainCount;
        end;
      end;
      writeln(' done in ',myTimeToStr(now-startOfCalculation));
      if length(toRemove)>0 then reInitTree(toRemove);
      writeln('Reduced geometry: ',length(tree.allObjects),' primitives');
    end;

  VAR it:longint;
      pendingChunks:T_pendingList;
      chunkCount:longint;
      chunksDone:longint=0;
      anyStarted:boolean;
      sleepTime:longint=0;

  begin
    if removeObjects<>rml_none then removeOffScrenObjects;
    initKdTree;
    if removeObjects=rml_retainOnlyDirectlyVisible then retainOnlyDirectlyVisible;

    if fileExists(dumpName) then begin
      write('DUMP FOUND ');
      renderImage.create(dumpName);
      if (renderImage.dimensions.width<>xRes) or (renderImage.dimensions.height<>yRes) then begin
        writeln('AND REJECTED DUE TO WRONG RESOLUTION');
        renderImage.resize(xRes,yRes,res_dataResize);
        markChunksAsPending(renderImage);
      end else write('AND ACCEPTED. RESUMING CALCULATION ');
    end else begin
      renderImage.create(xRes,yRes);
      markChunksAsPending(renderImage);
    end;
    {$ifdef debugLightingComponents}
    pathImage.create(xRes,yRes);
    dirImage .create(xRes,yRes);
    restImage.create(xRes,yRes);
    {$endif}

    chunkCount:=chunksInMap(xRes,yRes);
    if repairMode then pendingChunks:=getPendingListForRepair(renderImage)
                  else pendingChunks:=getPendingList         (renderImage);
    chunksDone:=chunkCount-length(pendingChunks);
    if chunksDone/chunkCount>=0.001 then writeln('@',chunksDone/chunkCount*100:0:2,'%');

    initProgress(chunksDone/chunkCount);
    for it:=0 to numberOfCPUs-1 do if length(pendingChunks)>0 then begin
      chunkToPrepare[it]:=pendingChunks[length(pendingChunks)-1];
      setLength(pendingChunks,length(pendingChunks)-1);
      {$ifdef UNIX} beginThread(@prepareChunk,@chunkToPrepare[it],renderThreadID[it]);
      {$else}       renderThreadID[it]:=beginThread(@prepareChunk,@chunkToPrepare[it]);
      {$endif}
    end else chunkToPrepare[it]:=-1;
    while length(pendingChunks)>0 do begin
      anyStarted:=false;
      for it:=0 to numberOfCPUs-1 do if (length(pendingChunks)>0) and (waitForThreadTerminate(renderThreadID[it],1)=0) then begin
        inc(chunksDone);
        chunkToPrepare[it]:=pendingChunks[length(pendingChunks)-1];
        setLength(pendingChunks,length(pendingChunks)-1);
        {$ifdef UNIX} beginThread(@prepareChunk,@chunkToPrepare[it],renderThreadID[it]);
        {$else}       renderThreadID[it]:=beginThread(@prepareChunk,@chunkToPrepare[it]);
        {$endif}
        anyStarted:=true;
        sleepTime:=1;
      end;
      if anyStarted then stepProgress(chunksDone/chunkCount)
                    else inc(sleepTime,1);
      sleep(sleepTime);
    end;
    writeln('waiting for the rest...');
    for it:=0 to numberOfCPUs-1 do if (chunkToPrepare[it]>=0) then begin
      repeat sleep(100) until (waitForThreadTerminate(renderThreadID[it],1)=0);
      inc(chunksDone);
      if chunksDone<chunkCount then stepProgress(chunksDone/chunkCount);
    end;

    tree.destroy;
    if keepDump then begin
      writeln('final DUMP: ',dumpName);
      renderImage.saveToFile(dumpName);
    end;
    {$ifdef debugLightingComponents}
    pathImage.saveToFile(dumpName('path')); pathImage.destroy;
    dirImage .saveToFile(dumpName('drct')); dirImage .destroy;
    restImage.saveToFile(dumpName('rest')); restImage.destroy;
    {$endif}

    if uppercase(extractFileExt(fileName))<>'.VRAW' then begin
      writeln('postprocessing');
      renderImage.shine;
    end;
    try
      renderImage.saveJpgWithSizeLimit(fileName,0);
    except
      renderImage.saveToFile(fileName);
    end;
    renderImage.destroy;
    writeln(fileName,' created in ',myTimeToStr(now-startOfCalculation));

    if fileExists(dumpName) and not(keepDump) then begin
      writeln('deleting DUMP');
      DeleteFile(dumpName);
    end;
  end;

PROCEDURE calculateImage(CONST removeObjects:T_removeObjectLevel=rml_none);
  VAR xRes       :longint=1920;
      yRes       :longint=1080;
      fileName   :string;
      repairMode :boolean=false;

  CONST cmdList:array [0..7] of T_commandAbstraction=(
    (isFile:true;  leadingSign:' '; cmdString:'';      paramCount: 0),  //0 file (for output)
    (isFile:false; leadingSign:'-'; cmdString:'';      paramCount: 2),  //1 resolution
    (isFile:false; leadingSign:'-'; cmdString:'t';     paramCount: 1),  //2 tolerance
    (isFile:false; leadingSign:'-'; cmdString:'r';     paramCount: 1),  //3 reflectionDepth
    (isFile:false; leadingSign:'-'; cmdString:'c';     paramCount: 1),  //4 number of CPUs
    (isFile:false; leadingSign:'-'; cmdString:'keepDump'; paramCount: 0),
    (isFile:false; leadingSign:'-'; cmdString:'lm'; paramCount: 1),     //6 lighting model
    (isFile:false; leadingSign:'-'; cmdString:'repair'; paramCount: 0));

  VAR i:longint;
      ep:T_extendedParameter;
      cmdLine:ansistring;
      lastCall:textFile;
  begin
    cmdLine:=extractFileName(paramStr(0))+' ';

    fileName:=ChangeFileExt(paramStr(0),'.jpg');
    for i:=1 to paramCount do begin
      cmdLine:=cmdLine+paramStr(i)+' ';
      ep:=extendedParam(i);
      case byte(matchingCmdIndex(ep,cmdList)) of
        0: fileName:=ep.cmdString;
        1: begin xRes:=ep.intParam[0]; yRes:=ep.intParam[1]; end;
        2: globalRenderTolerance:=ep.floatParam[0];
        3: reflectionDepth:=ep.intParam[0];
        4: numberOfCPUs:=ep.intParam[0];
        5: keepDump:=true;
        6: lighting.lightingModel:=ep.intParam[0];
        7: repairMode:=true;
      end;
    end;
    view.changeResolution(xRes,yRes);
    //for resuming:------------------------------------------------
    assign(lastCall,ChangeFileExt(paramStr(0),'.lastCall.bat'));
    rewrite(lastCall);
    writeln(lastCall,cmdLine);
    close(lastCall);
    //------------------------------------------------:for resuming
    writeln('output file          (#): ',fileName);
    writeln('output resolution (-#x#): ',xRes,'x',yRes);
    if globalRenderTolerance<=1E-3
    then writeln('render tolerance   (-t#): 0 [PREVIEW]')
    else writeln('render tolerance   (-t#): ',globalRenderTolerance:0:3);
    writeln('reflection depth   (-r#): ',reflectionDepth);
    writeln('number of threads  (-c#): ',numberOfCPUs);
    writeln('             (-keepDump): ',keepDump);
    case lighting.lightingModel of
      LIGHTING_MODEL_NOAMB                : writeln('Lighting model    (-lm#): ',lighting.lightingModel,' - SIMPLE (NO AMBIENT)');
      LIGHTING_MODEL_SIMPLE               : writeln('Lighting model    (-lm#): ',lighting.lightingModel,' - SIMPLE (RANDOMIZED AMBIENT OCCLUSION)');
      LIGHTING_MODEL_LAZY_PATH_TRACING    : writeln('Lighting model    (-lm#): ',lighting.lightingModel,' - LAZY PATH TRACING ');
      LIGHTING_MODEL_PATH_TRACING         : writeln('Lighting model    (-lm#): ',lighting.lightingModel,' - PATH TRACING');
      else                                  writeln('Lighting model    (-lm#): ',lighting.lightingModel,' --unknown--');
    end;
    writeln;
    writeln('Geometry: ',length(tree.allObjects),' primitives');
    writeln;
    writeln('Lighting: ambient  ',lighting.ambientLight[cc_red  ]:0:3,',',
                                  lighting.ambientLight[cc_green]:0:3,',',
                                  lighting.ambientLight[cc_blue ]:0:3);
    writeln('          points   ',length(lighting.pointLight));
    writeln('          backfunc ',(lighting.ambientFunc<>nil));
    if repairMode then begin
      writeln;
      writeln('------------------------------------------------');
      writeln('-----------  RUNNING IN REPAIR MODE  -----------');
      writeln('------------------------------------------------');
    end;
    calculateImage(xRes,yRes,repairMode,fileName,removeObjects);
  end;

CONSTRUCTOR I_traceableObject.init(mat:P_material);
  begin
    material:=mat;
  end;

FUNCTION I_traceableObject.rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean;
  VAR dummyHitDescription:T_hitDescription;
  begin
    result:=rayHits(ray,maxHitTime,dummyHitDescription) and
          ((ray.state and RAY_STATE_LAZY_LIGHT_SCAN=0) or
           (material^.getTransparencyLevel(dummyHitDescription.hitPoint)<random));
  end;

CONSTRUCTOR T_triangle.create(a,b,c:P_node; triMaterial:P_material);
  begin
    init(triMaterial);
    node[0]:=a;
    node[1]:=b;
    node[2]:=c;
  end;

DESTRUCTOR T_triangle.destroy;
  begin
  end;

FUNCTION T_triangle.rayHits(CONST ray:T_ray; CONST maxHitTime:double; OUT hitDescription:T_hitDescription):boolean;
  VAR by,bz,a,x:T_Vec3;
      invDet:double;
  begin
    result:=false;
    by:=node[0]^.position-node[1]^.position;
    bz:=node[0]^.position-node[2]^.position;
    invDet:=ray.direction[0]*by           [1]*bz           [2]-ray.direction[0]*bz           [1]*by           [2]
           +by           [0]*bz           [1]*ray.direction[2]-by           [0]*ray.direction[1]*bz           [2]
           +bz           [0]*ray.direction[1]*by           [2]-bz           [0]*by           [1]*ray.direction[2];
    if (abs(invDet)>1E-20) then begin
      invDet:=1/invDet;
      a:=node[0]^.position-ray.start;
      x[0]:=invDet*(a[0]*(by[1]*bz[2]-bz[1]*by[2])
                   +a[1]*(by[2]*bz[0]-bz[2]*by[0])
                   +a[2]*(by[0]*bz[1]-bz[0]*by[1]));
      if (x[0]>0) and (x[0]<maxHitTime) then begin
        x[1]:=invDet*(a[0]*(bz[1]*ray.direction[2]-bz[2]*ray.direction[1])
                     +a[1]*(bz[2]*ray.direction[0]-bz[0]*ray.direction[2])
                     +a[2]*(bz[0]*ray.direction[1]-bz[1]*ray.direction[0]));
        x[2]:=invDet*(a[0]*(by[2]*ray.direction[1]-by[1]*ray.direction[2])
                     +a[1]*(by[0]*ray.direction[2]-by[2]*ray.direction[0])
                     +a[2]*(by[1]*ray.direction[0]-by[0]*ray.direction[1]));
        if (x[1]>=0) and (x[2]>=0) and (x[1]+x[2]<=1) then with hitDescription do begin
          result:=true;
          hitTime :=x[0];
          hitPoint:=ray.start+ray.direction*hitTime;
          hitNormal:=normed((1-x[1]-x[2])*node[0]^.normal+
                               x[1]      *node[1]^.normal+
                                    x[2] *node[2]^.normal);
        end;
      end;
    end;
  end;

FUNCTION T_triangle.rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean;
  VAR by,bz,a,x:T_Vec3;
      invDet:double;
  begin
    result:=false;
    by:=node[0]^.position-node[1]^.position;
    bz:=node[0]^.position-node[2]^.position;
    invDet:=ray.direction[0]*by           [1]*bz           [2]-ray.direction[0]*bz           [1]*by           [2]
           +by           [0]*bz           [1]*ray.direction[2]-by           [0]*ray.direction[1]*bz           [2]
           +bz           [0]*ray.direction[1]*by           [2]-bz           [0]*by           [1]*ray.direction[2];
    if (abs(invDet)>NO_DIV_BY_ZERO_EPSILON) then begin
      invDet:=1/invDet;
      a:=node[0]^.position-ray.start;
      x[0]:=invDet*(a[0]*(by[1]*bz[2]-bz[1]*by[2])
                   +a[1]*(by[2]*bz[0]-bz[2]*by[0])
                   +a[2]*(by[0]*bz[1]-bz[0]*by[1]));
      if (x[0]>0) and (x[0]<maxHitTime) then begin
        x[1]:=invDet*(a[0]*(bz[1]*ray.direction[2]-bz[2]*ray.direction[1])
                     +a[1]*(bz[2]*ray.direction[0]-bz[0]*ray.direction[2])
                     +a[2]*(bz[0]*ray.direction[1]-bz[1]*ray.direction[0]));
        x[2]:=invDet*(a[0]*(by[2]*ray.direction[1]-by[1]*ray.direction[2])
                     +a[1]*(by[0]*ray.direction[2]-by[2]*ray.direction[0])
                     +a[2]*(by[1]*ray.direction[0]-by[0]*ray.direction[1]));
        if (x[1]>=0) and (x[2]>=0) and (x[1]+x[2]<=1) then begin
          result:=(ray.state and RAY_STATE_LAZY_LIGHT_SCAN=0) or (material^.getTransparencyLevel(ray.start+ray.direction*x[0])<random);
        end;
      end;
    end;
  end;

FUNCTION T_triangle.isContainedInBox(CONST box:T_boundingBox):boolean;
  begin
    result:=box.intersectsTriangle(node[0]^.position,node[1]^.position,node[2]^.position);
  end;

FUNCTION T_triangle.getBoundingBox:T_boundingBox;
  begin
    result.create(node[0]^.position,node[1]^.position,node[2]^.position);
  end;

FUNCTION T_triangle.getSamplingPoint:T_Vec3;
  VAR u,v:double;
  begin
    repeat
      u:=random;
      v:=random;
    until u+v<1;
    result:=node[0]^.position+u*(node[1]^.position-node[0]^.position)
                             +v*(node[2]^.position-node[0]^.position);
  end;

CONSTRUCTOR T_FlatTriangle.create(a,b,c:T_Vec3; triMaterial:P_material);
  begin
    init(triMaterial);
    node[0]:=a;
    node[1]:=b;
    node[2]:=c;
    normal:=normed(cross(b-a,c-a));
  end;

DESTRUCTOR T_FlatTriangle.destroy;
  begin
  end;

FUNCTION T_FlatTriangle.rayHits(CONST ray:T_ray; CONST maxHitTime:double; OUT hitDescription:T_hitDescription):boolean;
  VAR by,bz,a,x:T_Vec3;
      invDet:double;
  begin
    result:=false;
    by:=node[0]-node[1];
    bz:=node[0]-node[2];
    invDet:=ray.direction[0]*by           [1]*bz           [2]-ray.direction[0]*bz           [1]*by           [2]
           +by           [0]*bz           [1]*ray.direction[2]-by           [0]*ray.direction[1]*bz           [2]
           +bz           [0]*ray.direction[1]*by           [2]-bz           [0]*by           [1]*ray.direction[2];
    if (abs(invDet)>NO_DIV_BY_ZERO_EPSILON) then begin
      invDet:=1/invDet;
      a:=node[0]-ray.start;
      x[0]:=invDet*(a[0]*(by[1]*bz[2]-bz[1]*by[2])
                   +a[1]*(by[2]*bz[0]-bz[2]*by[0])
                   +a[2]*(by[0]*bz[1]-bz[0]*by[1]));
      if (x[0]>0) and (x[0]<maxHitTime) then begin
        x[1]:=invDet*(a[0]*(bz[1]*ray.direction[2]-ray.direction[1]*bz[2])
                     +a[1]*(bz[2]*ray.direction[0]-ray.direction[2]*bz[0])
                     +a[2]*(bz[0]*ray.direction[1]-ray.direction[0]*bz[1]));
        x[2]:=invDet*(a[0]*(ray.direction[1]*by[2]-by[1]*ray.direction[2])
                     +a[1]*(ray.direction[2]*by[0]-by[2]*ray.direction[0])
                     +a[2]*(ray.direction[0]*by[1]-by[0]*ray.direction[1]));
        if (x[1]>=0) and (x[2]>=0) and (x[1]+x[2]<=1) then with hitDescription do begin
          if (material^.getTransparencyLevel(ray.start+ray.direction*hitTime)=1) and (material^.relRefractionIdx=1) then exit(false);
          result:=true;
          hitTime :=x[0];
          hitPoint:=ray.start+ray.direction*hitTime;
          hitNormal:=normal;
        end;
      end;
    end;
  end;

FUNCTION T_FlatTriangle.rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean;
  VAR by,bz,a,x:T_Vec3;
      invDet:double;
  begin
    result:=false;
    by:=node[0]-node[1];
    bz:=node[0]-node[2];
    invDet:=ray.direction[0]*by           [1]*bz           [2]-ray.direction[0]*bz           [1]*by           [2]
           +by           [0]*bz           [1]*ray.direction[2]-by           [0]*ray.direction[1]*bz           [2]
           +bz           [0]*ray.direction[1]*by           [2]-bz           [0]*by           [1]*ray.direction[2];
    if (abs(invDet)>NO_DIV_BY_ZERO_EPSILON) then begin
      invDet:=1/invDet;
      a:=node[0]-ray.start;
      x[0]:=invDet*(a[0]*(by[1]*bz[2]-bz[1]*by[2])
                   +a[1]*(by[2]*bz[0]-bz[2]*by[0])
                   +a[2]*(by[0]*bz[1]-bz[0]*by[1]));
      if (x[0]>0) and (x[0]<maxHitTime) then begin
        x[1]:=invDet*(a[0]*(bz[1]*ray.direction[2]-bz[2]*ray.direction[1])
                     +a[1]*(bz[2]*ray.direction[0]-bz[0]*ray.direction[2])
                     +a[2]*(bz[0]*ray.direction[1]-bz[1]*ray.direction[0]));
        x[2]:=invDet*(a[0]*(by[2]*ray.direction[1]-by[1]*ray.direction[2])
                     +a[1]*(by[0]*ray.direction[2]-by[2]*ray.direction[0])
                     +a[2]*(by[1]*ray.direction[0]-by[0]*ray.direction[1]));
        if (x[1]>=0) and (x[2]>=0) and (x[1]+x[2]<=1) then begin
          result:=(ray.state and RAY_STATE_LAZY_LIGHT_SCAN=0) or (material^.getTransparencyLevel(ray.start+ray.direction*x[0])<random);
        end;
      end;
    end;
  end;

FUNCTION T_FlatTriangle.isContainedInBox(CONST box:T_boundingBox):boolean;
  begin
    result:=box.intersectsTriangle(node[0],node[1],node[2]);
  end;

FUNCTION T_FlatTriangle.getBoundingBox:T_boundingBox;
  begin
    result.create(node[0],node[1],node[2]);
  end;

FUNCTION T_FlatTriangle.getSamplingPoint:T_Vec3;
  VAR u,v:double;
  begin
    repeat
      u:=random;
      v:=random;
    until u+v<1;
    result:=node[0]+u*(node[1]-node[0])
                   +v*(node[2]-node[0]);
  end;

CONSTRUCTOR T_axisParallelQuad.create(c1,c2:T_Vec3; mat:P_material);
  begin
    init(mat);
    qBox.create(c1,c2);
  end;

DESTRUCTOR T_axisParallelQuad.destroy;
  begin
    qBox.destroy;
  end;

FUNCTION T_axisParallelQuad.rayHits(CONST ray:T_ray; CONST maxHitTime:double; OUT hitDescription:T_hitDescription):boolean;
  VAR invDir:double;
      trans:array [-1..5] of record
        axis:char;
        t:double;
      end;
      i,j:longint;
      inFlag:array['x'..'z'] of boolean;
  begin
    result:=false;
    if abs(ray.direction[0])>NO_DIV_BY_ZERO_EPSILON then begin
      invDir:=ray.invDir[0];
      trans[0].t:=(qBox.lower[0]-ray.start[0])*invDir;
      trans[1].t:=(qBox.upper[0]-ray.start[0])*invDir;
      if (trans[0].t<0) and (trans[1].t<0) then exit(false);
    end else exit(false);
    if abs(ray.direction[1])>NO_DIV_BY_ZERO_EPSILON then begin
      invDir:=ray.invDir[1];
      trans[2].t:=(qBox.lower[1]-ray.start[1])*invDir;
      trans[3].t:=(qBox.upper[1]-ray.start[1])*invDir;
      if (trans[2].t<0) and (trans[3].t<0) then exit(false);
    end else exit(false);
    if abs(ray.direction[2])>NO_DIV_BY_ZERO_EPSILON then begin
      invDir:=ray.invDir[2];
      trans[4].t:=(qBox.lower[2]-ray.start[2])*invDir;
      trans[5].t:=(qBox.upper[2]-ray.start[2])*invDir;
      if (trans[4].t<0) and (trans[5].t<0) then exit(false);
    end else exit(false);
    trans[0].axis:='x';
    trans[1].axis:='x';
    trans[2].axis:='y';
    trans[3].axis:='y';
    trans[4].axis:='z';
    trans[5].axis:='z';
    for i:=1 to 5 do for j:=0 to i-1 do if trans[i].t<trans[j].t then begin
      trans[-1]:=trans[i]; trans[i]:=trans[j]; trans[j]:=trans[-1];
    end;
    inFlag['x']:=false;
    inFlag['y']:=false;
    inFlag['z']:=false;
    i:=-1;
    while (i<5) and not(inFlag['x'] and inFlag['y'] and inFlag['z']) do begin
      inc(i);
      inFlag[trans[i].axis]:=not(inFlag[trans[i].axis]);
    end;
    if (i<5) then begin
      if (material^.getTransparencyLevel(ray.start+ray.direction*trans[i].t)=1) and (material^.relRefractionIdx=1) then exit(false);
      if not((trans[i].t>0) and (trans[i].t<maxHitTime)) then inc(i);
      if (trans[i].t>0) and (trans[i].t<maxHitTime) then begin
        with hitDescription do begin
          hitTime:=trans[i].t;
          hitPoint:=ray.start+ray.direction*hitTime;
          case trans[i].axis of
            'x':hitNormal:=unitVecX;
            'y':hitNormal:=unitVecY;
            'z':hitNormal:=unitVecZ;
          end;
        end;
        exit(true);
      end;
    end;
    result:=false;
  end;

FUNCTION T_axisParallelQuad.rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean;
  VAR invDir:double;
      trans:array [-1..5] of record
        axis:char;
        t:double;
      end;
      i,j:longint;
      inFlag:array['x'..'z'] of boolean;
  begin
    result:=false;
    if abs(ray.direction[0])>NO_DIV_BY_ZERO_EPSILON then begin
      invDir:=ray.invDir[0];
      trans[0].t:=(qBox.lower[0]-ray.start[0])*invDir;
      trans[1].t:=(qBox.upper[0]-ray.start[0])*invDir;
      if (trans[0].t<0) and (trans[1].t<0) then exit(false);
    end else exit(false);
    if abs(ray.direction[1])>NO_DIV_BY_ZERO_EPSILON then begin
      invDir:=ray.invDir[1];
      trans[2].t:=(qBox.lower[1]-ray.start[1])*invDir;
      trans[3].t:=(qBox.upper[1]-ray.start[1])*invDir;
      if (trans[2].t<0) and (trans[3].t<0) then exit(false);
    end else exit(false);
    if abs(ray.direction[2])>NO_DIV_BY_ZERO_EPSILON then begin
      invDir:=ray.invDir[2];
      trans[4].t:=(qBox.lower[2]-ray.start[2])*invDir;
      trans[5].t:=(qBox.upper[2]-ray.start[2])*invDir;
      if (trans[4].t<0) and (trans[5].t<0) then exit(false);
    end else exit(false);
    trans[0].axis:='x';
    trans[1].axis:='x';
    trans[2].axis:='y';
    trans[3].axis:='y';
    trans[4].axis:='z';
    trans[5].axis:='z';
    for i:=1 to 5 do for j:=0 to i-1 do if trans[i].t<trans[j].t then begin
      trans[-1]:=trans[i]; trans[i]:=trans[j]; trans[j]:=trans[-1];
    end;
    inFlag['x']:=false;
    inFlag['y']:=false;
    inFlag['z']:=false;
    i:=-1;
    while (i<5) and not(inFlag['x'] and inFlag['y'] and inFlag['z']) do begin
      inc(i);
      inFlag[trans[i].axis]:=not(inFlag[trans[i].axis]);
    end;
    if (i<5) then begin
      if (material^.getTransparencyLevel(ray.start+ray.direction*trans[i].t)=1) and (material^.relRefractionIdx=1) then exit(false);
      if (trans[i].t>0) and (trans[i].t<maxHitTime) and ((ray.state and RAY_STATE_LAZY_LIGHT_SCAN=0) or (material^.getTransparencyLevel(ray.start+ray.direction*trans[i].t)<random)) then begin
        exit(true);
      end else begin
        inc(i);
        if (trans[i].t>0) and (trans[i].t<maxHitTime) and ((ray.state and RAY_STATE_LAZY_LIGHT_SCAN=0) or (material^.getTransparencyLevel(ray.start+ray.direction*trans[i].t)<random)) then begin
          exit(true);
        end;
      end;
    end;
    result:=false;
  end;

FUNCTION T_axisParallelQuad.isContainedInBox(CONST box:T_boundingBox):boolean;
  begin
    result:=box.intersects(qBox);
  end;

FUNCTION T_axisParallelQuad.getBoundingBox:T_boundingBox;
  begin
    result:=qBox;
  end;

FUNCTION T_axisParallelQuad.getSamplingPoint:T_Vec3;
  VAR u,v,w:double;
  begin
    u:=random;
    v:=random;
    w:=random;
    case random(6) of
      0: u:=0; 1: u:=1;
      2: v:=0; 3: v:=1;
      4: w:=0; 5: w:=1;
    end;
    result[0]:=qBox.lower[0]+u*(qBox.upper[0]-qBox.lower[0]);
    result[1]:=qBox.lower[1]+v*(qBox.upper[1]-qBox.lower[1]);
    result[2]:=qBox.lower[2]+w*(qBox.upper[2]-qBox.lower[2]);
  end;

CONSTRUCTOR T_sphere.create(sphereCenter:T_Vec3; sphereRadius:double; mat:P_material);
  begin
    init(mat);
    center:=sphereCenter;
    radius:=sphereRadius;
  end;

DESTRUCTOR T_sphere.destroy;
  begin end;

FUNCTION T_sphere.rayHits(CONST ray:T_ray; CONST maxHitTime:double; OUT hitDescription:T_hitDescription):boolean;
  VAR inRoot:double;
      relCenter:T_Vec3;
      dc:double;
      t0,t1:double;
  begin
    relCenter:=center-ray.start; //relative center
    inRoot:=system.sqr(ray.direction*relCenter)+(radius*radius-sqNorm(relCenter));
    if inRoot<=0 then exit(false);

    inRoot:=sqrt(inRoot); //always: >0
    dc:=ray.direction*relCenter;
    t0:=dc-inRoot;
    t1:=dc+inRoot; //always >= t0
    if (t0>0) and (t0<maxHitTime) and not((material^.getTransparencyLevel(ray.start+ray.direction*t0)=1) and (material^.relRefractionIdx=1)) then begin
      result:=true; hitDescription.hitTime:=t0;
    end else if (t1>0) and (t1<maxHitTime) and not((material^.getTransparencyLevel(ray.start+ray.direction*t1)=1) and (material^.relRefractionIdx=1)) then begin
      result:=true; hitDescription.hitTime:=t1;
    end else result:=false;
    if result then with hitDescription do begin
      hitPoint:=ray.start+ray.direction*hitTime;
      hitNormal:=(center-hitPoint)*(1/radius);
    end;
  end;

FUNCTION T_sphere.rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean;
  VAR inRoot,t0,t1,dc:double;
      relCenter:T_Vec3;
  begin
    relCenter:=center-ray.start; //relative center
    inRoot:=system.sqr(ray.direction*relCenter)+(radius*radius-sqNorm(relCenter));
    if inRoot<=0 then exit(false);
    inRoot:=sqrt(inRoot); //always: >0
    dc:=ray.direction*relCenter;
    t0:=dc-inRoot;
    t1:=dc+inRoot;
    if (t0>0) and (t0<maxHitTime) and ((ray.state and RAY_STATE_LAZY_LIGHT_SCAN=0) or (material^.getTransparencyLevel(ray.start+ray.direction*t0)<random)) then begin
      result:=true;
    end else if (t1>0) and (t1<maxHitTime) and ((ray.state and RAY_STATE_LAZY_LIGHT_SCAN=0) or (material^.getTransparencyLevel(ray.start+ray.direction*t1)<random)) then begin
      result:=true;
    end else result:=false;
  end;

FUNCTION T_sphere.isContainedInBox(CONST box:T_boundingBox):boolean;
  begin
    result:=box.intersectsSphere(center,radius);
  end;

FUNCTION T_sphere.getBoundingBox:T_boundingBox;
  begin
    result.create(center,radius);
  end;

FUNCTION T_sphere.getSamplingPoint:T_Vec3;
  begin
    result:=center+randomVecOnUnitSphere*radius;
  end;

INITIALIZATION
  samplingStatistics:=zeroSamplingStatistics;
  currentSamplingStatistics:=zeroSamplingStatistics;
end.
