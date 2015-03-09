UNIT raytrace;
INTERFACE
USES cmdLineParseUtil,mypics,math,linAlg3d,sysutils,darts,myGenerics,picChunks,lightMaps;
CONST
  integ:array[-1..15] of longint=(-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);  
  
  LIGHTING_MODEL_NOAMB                =0;
  LIGHTING_MODEL_IGNORANT             =1;
  LIGHTING_MODEL_SIMPLE               =2;
  LIGHTING_MODEL_LAZY_PATH_TRACING    =3;
  LIGHTING_MODEL_PATH_TRACING         =4;
  LIGHTING_MODEL_OCCLUSION_MAP        =5;  
  LIGHTING_MODEL_PATH_LIGHT_MAP       =6;
    
VAR maxObjectsPerOctreeNode:longint=16;
    handDownThreshold:longint=2;
TYPE
  P_material=^T_material;
  P_traceableObject=^I_traceableObject;

  FT_getLightCallback=FUNCTION():T_pointLightInstance;
  T_pointLight=object
    pos:T_Vec3;
    distort:double;
    col:T_floatColor;
    infiniteDist:boolean;
    brightnessCap:double;
    func:FT_getLightCallback;

    CONSTRUCTOR create(p:T_Vec3; c:T_floatColor; d:double; infDist:boolean; cap:double; callback:FT_getLightCallback);
    FUNCTION getInstance(CONST surfaceNormal:T_Vec3):T_pointLightInstance;
    FUNCTION getLookIntoLight        (CONST ray:T_ray; CONST tMax:double; OUT specularMask:byte):T_floatColor;        
    FUNCTION getLookIntoLightIntegral(CONST undistortedRay:T_ray; CONST hitNormal:T_Vec3; CONST distortion:double; CONST tMax:double; VAR specularMask:byte):T_floatColor;  
  end;

  FT_colorOfPosCallback=FUNCTION (normal:T_Vec3):T_floatColor;
  FT_doubleOfPosCallback=FUNCTION (point:T_Vec3):double;

  T_hitDescription=record
    hitTime:double;
    hitPoint,
    hitNormal:T_Vec3;    
    hitMaterialPoint:T_materialPoint;
    hitObject:P_traceableObject;
  end;

  T_lighting=object
    ambientLight:T_floatColor;
    pointLight:array of T_pointLight;
    ambientFunc:FT_colorOfPosCallback;
    lightingModel:byte;
    specularLights:boolean;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE addPointLight(position:T_Vec3; color:T_floatColor);
    PROCEDURE addPointLight(position:T_Vec3; distortion:double; color:T_floatColor);
    PROCEDURE addPositionedLight(position:T_Vec3; color:T_floatColor);
    PROCEDURE addPositionedLight(position:T_Vec3; distortion:double; color:T_floatColor);
    PROCEDURE addPositionedLight(position:T_Vec3; distortion:double; color:T_floatColor; brightnessCap:double);
    PROCEDURE addLight(lightDef:T_pointLight);
    FUNCTION getInstance(CONST index:longint; CONST surfaceNormal:T_Vec3):T_pointLightInstance;    
    FUNCTION getBackground(CONST dir:T_Vec3):T_floatColor;
    FUNCTION getLookIntoLight(CONST ray:T_ray; CONST tMax:double):T_floatColor;
    FUNCTION getGlobalLME:T_lightMapElement;  
    //FUNCTION getIlluminatingRay(CONST geometryCenter:T_Vec3; CONST geometryRadius:double):T_ray;
  end;

  T_material=object
    diffuseColor  :T_floatColor;
    diffuseFunc   :FT_colorOfPosCallback;

    reflectiveness:T_floatColor;
    reflectFunc   :FT_colorOfPosCallback;
    reflectDistortion:double;
    reflectDistFunc:FT_doubleOfPosCallback;

    glow:T_floatColor;
    glowFunc:FT_colorOfPosCallback;

    transparency:T_floatColor;
    transparencyFunc:FT_colorOfPosCallback;
    relRefractionIdx:double;
    refractDistortion:double;

    CONSTRUCTOR create(baseR,baseG,baseB:double);
    CONSTRUCTOR create(c:T_floatColor);
    DESTRUCTOR destroy;
    FUNCTION getMaterialPoint(CONST position:T_Vec3):T_materialPoint;    
    FUNCTION getTransparencyLevel(CONST position:T_Vec3):single;
  end;

  I_traceableObject=object
    material:P_material;
    CONSTRUCTOR init(mat:P_material);
    DESTRUCTOR destroy; virtual; abstract;
    FUNCTION rayHits(CONST ray:T_ray; CONST maxHitTime:double; OUT hitDescription:T_hitDescription):boolean; virtual; abstract;
    FUNCTION rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean; virtual; abstract;
    FUNCTION isContainedInBox(CONST box:T_boundingBox):boolean; virtual; abstract;
    FUNCTION getBoundingBox:T_boundingBox; virtual; abstract;
  end;

  P_triangle=^T_Triangle;
  T_triangle=object(I_traceableObject)
    node:array[0..2] of P_Node;
    CONSTRUCTOR create(a,b,c:P_node; triMaterial:P_material);
    DESTRUCTOR destroy; virtual;
    FUNCTION rayHits(CONST ray:T_ray; CONST maxHitTime:double; OUT hitDescription:T_hitDescription):boolean; virtual;
    FUNCTION rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean; virtual;
    FUNCTION isContainedInBox(CONST box:T_boundingBox):boolean; virtual;
    FUNCTION getBoundingBox:T_boundingBox; virtual;
  end;

  P_FlatTriangle=^T_FlatTriangle;
  T_FlatTriangle=object(I_traceableObject)
    node:array[0..2] of T_Vec3;
    normal:T_vec3;
    CONSTRUCTOR create(a,b,c:T_Vec3; triMaterial:P_material);
    DESTRUCTOR destroy; virtual;
    FUNCTION rayHits(CONST ray:T_ray; CONST maxHitTime:double; OUT hitDescription:T_hitDescription):boolean; virtual;
    FUNCTION rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean; virtual;
    FUNCTION isContainedInBox(CONST box:T_boundingBox):boolean; virtual;
    FUNCTION getBoundingBox:T_boundingBox; virtual;
  end;


  P_axisParallelQuad=^T_axisParallelQuad;
  T_axisParallelQuad=object(I_traceableObject)
    qbox:T_boundingBox;
    CONSTRUCTOR create(c1,c2:T_Vec3; mat:P_material);
    DESTRUCTOR destroy; virtual;
    FUNCTION rayHits(CONST ray:T_ray; CONST maxHitTime:double; OUT hitDescription:T_hitDescription):boolean; virtual;
    FUNCTION rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean; virtual;
    FUNCTION isContainedInBox(CONST box:T_boundingBox):boolean; virtual;
    FUNCTION getBoundingBox:T_boundingBox; virtual;
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
  end;
  
  P_octree=^T_Octree;
  T_octree=object
    obj:array of P_traceableObject;
    subtrees:array[0..1,0..1,0..1] of P_octree;

    CONSTRUCTOR create;
    CONSTRUCTOR createSuperNode(forNode:P_octree; i,j,k:longint);
    DESTRUCTOR destroy;
    FUNCTION addObject(CONST box:T_boundingBox; CONST o:P_traceableObject; CONST depth:byte):boolean;
    FUNCTION rayHitsObjectInTree(CONST lowerHitTime,upperHitTime:T_Vec3; VAR ray:T_ray; OUT hitDescription:T_hitDescription):boolean;
    FUNCTION rayHitsObjectInTreeInaccurate(VAR lowerHitTime,upperHitTime:T_Vec3; CONST ray:T_ray; maxHitTime:double):boolean;
    FUNCTION containsObject(CONST o:P_traceableObject):boolean;
    PROCEDURE removeObject(CONST o:P_traceableObject);
    FUNCTION redistributeObjects:longint;
  end;
  

  T_octreeRoot=object
    rayCount:longint;
  
    allObjects:array of P_traceableObject;
    allNodes:array of P_Node;
    box:T_boundingBox;
    tree:P_octree;
    
    maxDepth:byte;
    basePlane:record
      present:boolean;
      yPos:double;
      material:P_material;
    end;
    
    fog:record
      present:boolean;
      density:double;
      yPos:array[0..2] of double;
      transmittanceFunc:FT_doubleOfPosCallback;
      material:P_material;      
    end;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE registerNode(node:P_node);
    PROCEDURE addObject(obj:P_traceableObject);
    PROCEDURE addFlatTriangle(a,b,c:T_Vec3; material:P_material);
    PROCEDURE changeDepth(newDepth:byte);
    PROCEDURE initialize(calc:FT_calcNodeCallback; u0,u1:double; uSteps:longint; v0,v1:double; vSteps:longint; material:P_Material);
    PROCEDURE initialize(calc:FT_calcNodeCallback; VAR view:T_view; avgWorldY:double; Steps:longint; material:P_Material);
    PROCEDURE initialize(calc:FT_calcNodeCallback; u0,u1,v0,v1:double; nodeCount,fixedUNodes,fixedVNodes:longint; material:P_Material);
    PROCEDURE initialize(calc:FT_calcNodeCallback; u0,u1,v0,v1:double; nodeCount:longint; material:P_Material);
    PROCEDURE initializeLocRef(calc:FT_calcNodeCallback; u0,u1:double; iu0:longint; v0,v1:double; iv0:longint; splitThreshold:double; material:P_Material);
    PROCEDURE initializeFacets(calc:FT_calcNodeCallback; u0,u1:double; iu0:longint; v0,v1:double; iv0:longint; splitThreshold:double; material:P_Material);
    FUNCTION rayHitsObjectInTree(VAR ray:T_ray; OUT hitDescription:T_hitDescription; CONST inaccurate:boolean; CONST tMax:double):boolean;
    FUNCTION lightVisibility(CONST hit:T_hitDescription; CONST lazy:boolean; CONST light:T_pointLight; CONST undistortedRay:T_ray; VAR shadowByte:byte):T_floatColor; inline;
    FUNCTION getHitColor(VAR ray:T_ray; CONST depth:byte; VAR lightMap:T_lightMap):T_floatColor;
    PROCEDURE getHitColor(CONST pixelX,pixelY:longint; CONST firstRun:boolean; VAR lightMap:T_lightMap; VAR colors:T_structuredHitColor);
    
    PROCEDURE fillLightMapElement(VAR lightMap:T_lightMap; VAR element:T_lightMapElement; CONST hitPoint,hitNormal:T_Vec3);    
    
    PROCEDURE addBasePlane(y:double; material:P_material);
    PROCEDURE setFog(lowY,maxY,highY,density:double; material:P_material; transmittance:FT_doubleOfPosCallback);
    
  end;

PROCEDURE calculateImage;

VAR
  reflectionDepth:longint=2;
  renderImage:T_floatMap;
  view:T_View;
  lighting:T_lighting;
  tree:T_octreeRoot;
  renderThreadID:array[0..15] of TThreadID;
  chunkToPrepare:array[0..15] of longint;
  ignorantLightingLME:T_lightMapElement;   
  
  lightMapGridSize:double=0.02;//015;
  lightMapQuality:byte=0;
   
  numberOfCPUs:longint=4;
  keepDump:boolean=false;
  globalRenderTolerance:double=1;

  
  
IMPLEMENTATION
CONSTRUCTOR T_pointLight.create(p:T_Vec3; c:T_floatColor; d:double; infDist:boolean; cap:double; callback:FT_getLightCallback);
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
    
FUNCTION T_pointLight.getLookIntoLight(CONST ray:T_ray; CONST tMax:double; OUT specularMask:byte):T_floatColor;
  FUNCTION sphereIntersection_finiteDist(CONST center:T_Vec3; CONST distRad:double):double; inline;
    VAR inRoot:double;
        relCenter:T_Vec3;
        dc,radius:double;
        t0,t1:double;  
    begin
      if distRad<1E-2 then radius:=1E-2
                      else radius:=distRad;    
      relCenter:=center-ray.start; //relative center
      inRoot:=system.sqr(ray.direction*relCenter)+(radius*radius-sqnorm(relCenter));
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
      inRoot:=(1-sqnorm(relCenter)/(radius*radius));
      if inRoot<=0 then begin specularMask:=specularMask or SPECMASK_SHADOW; result:=0; end
                   else begin specularMask:=specularMask or SPECMASK_LIGHT;  result:=(2*sqrt(inRoot))/(radius*radius*pi*4/3); end;
    end;
    
  VAR LI:T_pointLightInstance;
      i:longint;
  begin 
    specularMask:=SPECMASK_NONE;
    if func=nil then begin
      if infiniteDist
      then begin
        if tMax<9E19 
          then result:=black //not looking into infinity -> light in infinite distance is not seen
          else result:=col*sphereIntersection_infiniteDist(pos,distort)
      end else result:=col*sphereIntersection_finiteDist  (pos,distort);        
    end else begin
      result:=black;
      if infiniteDist then begin 
        if tMax<9E19 then result:=black //not looking into infinity -> light in infinite distance is not seen
        else for i:=0 to 9 do begin
          LI:=func();
          result:=result+0.1*LI.col*sphereIntersection_infiniteDist(LI.pos,0)
        end;
      end else for i:=0 to 9 do begin
        LI:=func();
        result:=result+0.1*LI.col*sphereIntersection_finiteDist  (LI.pos,0);              
      end;
    end;
  end;
  
FUNCTION T_pointLight.getLookIntoLightIntegral(CONST undistortedRay:T_ray; CONST hitNormal:T_Vec3; CONST distortion:double; CONST tMax:double; VAR specularMask:byte):T_floatColor;  
  VAR i,iMax:longint;
      ray:T_ray;
      maskAid:byte;
  begin
    result:=black;
    if (specularMask and SPECMASK_BOTH)=SPECMASK_BOTH then iMax:=64 else iMax:=8;
    if (specularMask and SHADOWMASK_BOTH)=SHADOWMASK_BOTH then iMax:=iMax shr 2;
    for i:=1 to iMax do begin
      ray:=undistortedRay;    
      ray.modifyReflected(hitNormal,distortion);
      result:=result+getLookIntoLight(ray,tMax,maskAid);
      specularMask:=specularMask or maskAid;
    end;
    result:=result*(1/iMax);
  end;

CONSTRUCTOR T_lighting.create;
  begin
    specularLights:=false;
    setLength(pointLight,0);
    lightingModel:=LIGHTING_MODEL_SIMPLE;
    ambientLight:=black;
    ambientFunc:=nil;
  end;

DESTRUCTOR T_lighting.destroy;
  begin
    setLength(pointLight,0);
  end;

PROCEDURE T_lighting.addPointLight(position:T_Vec3; distortion:double; color:T_floatColor);
  begin
    setLength(pointLight,length(pointLight)+1);
    pointLight[length(pointLight)-1].create(position,color,distortion,true,1,nil);
  end;

PROCEDURE T_lighting.addPositionedLight(position:T_Vec3; distortion:double; color:T_floatColor; brightnessCap:double);
  begin
    setLength(pointLight,length(pointLight)+1);
    pointLight[length(pointLight)-1].create(position,color,distortion,false,brightnessCap,nil);
  end;

PROCEDURE T_lighting.addPositionedLight(position:T_Vec3; distortion:double; color:T_floatColor);
  begin addPositionedLight(position,distortion,color,1E6); end;
PROCEDURE T_lighting.addPointLight(position:T_Vec3; color:T_floatColor);
  begin addPointLight(position,0,color); end;
PROCEDURE T_lighting.addPositionedLight(position:T_Vec3; color:T_floatColor);
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
  
FUNCTION T_lighting.getBackground(CONST dir:T_Vec3):T_floatColor;
  begin
    if ambientFunc=nil then result:=ambientLight
                       else result:=ambientFunc(dir);
  end;
    
FUNCTION T_lighting.getLookIntoLight(CONST ray:T_ray; CONST tMax:double):T_floatColor;
  VAR i:longint;
      dummyMask:byte;
  begin    
    result:=black;
    if not(specularLights) then for i:=0 to length(pointLight)-1 do begin
      dummyMask:=SPECMASK_NONE;
      result:=result+pointLight[i].getLookIntoLight(ray,tMax,dummyMask);
    end;
  end;
  
FUNCTION T_lighting.getGlobalLME:T_lightMapElement;    
  VAR i:longint;
  begin
    result.createLevelless(0);
    if ambientFunc=nil then begin      
      result.setColorForNormal(newVector( 1,0,0),ambientLight);
      result.setColorForNormal(newVector(-1,0,0),ambientLight);
      result.setColorForNormal(newVector(0, 1,0),ambientLight);
      result.setColorForNormal(newVector(0,-1,0),ambientLight);
      result.setColorForNormal(newVector(0,0, 1),ambientLight);
      result.setColorForNormal(newVector(0,0,-1),ambientLight);      
    end else begin    
      for i:=0 to 41 do result.setColorForNormal(FACET[i],ambientFunc(FACET[i]));
    end;
  end;
    
CONSTRUCTOR T_material.create(baseR,baseG,baseB:double);
  begin
    create(newColor(baseR,baseG,baseB));
  end;

CONSTRUCTOR T_material.create(c:T_floatColor);
  begin
    diffuseColor  :=c;
    diffuseFunc   :=nil;

    reflectiveness:=black;
    reflectFunc   :=nil;
    reflectDistortion:=0;
    reflectDistFunc:=nil;

    glow:=black;
    glowFunc:=nil;

    transparency:=black;
    transparencyFunc:=nil;
    relRefractionIdx:=1.4;
    refractDistortion:=0;
  end;

DESTRUCTOR T_material.destroy;
  begin

  end;
  
FUNCTION T_material.getMaterialPoint(CONST position:T_Vec3):T_materialPoint;    
  FUNCTION NVL(CONST func:FT_colorOfPosCallback;  CONST pos:T_Vec3; CONST col:T_floatColor):T_floatColor; inline; begin if func=nil then result:=col else result:=func(pos); end;  
  FUNCTION NVL(CONST func:FT_doubleOfPosCallback; CONST pos:T_Vec3; CONST col:double      ):double;       inline; begin if func=nil then result:=col else result:=func(pos); end;

  begin
    result.create(
      NVL(diffuseFunc     ,position,diffuseColor),
      NVL(glowFunc        ,position,glow),
      NVL(transparencyFunc,position,transparency),
      NVL(reflectFunc     ,position,reflectiveness),
      NVL(reflectDistFunc ,position,reflectDistortion),
      refractDistortion,
      relRefractionIdx);
  end;
  
FUNCTION T_material.getTransparencyLevel(CONST position:T_Vec3):single;  
  begin
    if transparencyFunc=nil then result:=greyLevel(transparency) else result:=greyLevel(transparencyFunc(position));
  end;

CONSTRUCTOR T_octree.create;
  VAR i,j,k:longint;
  begin
    setLength(obj,0);
    for i:=0 to 1 do for j:=0 to 1 do for k:=0 to 1 do subTrees[i,j,k]:=nil;
  end;

CONSTRUCTOR T_octree.createSuperNode(forNode:P_octree; i,j,k:longint);
  VAR ii,jj,kk:longint;
  begin
    setLength(obj,0);
    for ii:=0 to 1 do for jj:=0 to 1 do for kk:=0 to 1 do
    if (ii=i)        and (jj=j)        and (kk=k)
      then     subTrees[ii,jj,kk]:=forNode
      else new(subTrees[ii,jj,kk],create);
  end;

DESTRUCTOR T_octree.destroy;
  VAR i,j,k:longint;
  begin
    setLength(obj,0);
    for i:=0 to 1 do for j:=0 to 1 do for k:=0 to 1 do
      if subTrees[i,j,k]<>nil then begin
        dispose(subTrees[i,j,k],destroy); subTrees[i,j,k]:=nil;
      end;
  end;

FUNCTION T_octree.addObject(CONST box:T_boundingBox; CONST o:P_traceableObject; CONST depth:byte):boolean;
  PROCEDURE handDown(CONST o:P_traceableObject); inline;
    VAR L,Mask,count:byte;
    begin
      mask:=0;
      count:=0;
      for L:=0 to 7 do if o^.isContainedInBox(
        box.subBox((L shr 2) and 1,
                   (L shr 1) and 1,
                    L        and 1)) then begin inc(mask,1 shl L); inc(count); end;
      if (mask=255) or (count>=handDownThreshold) then begin
        setLength(obj,length(obj)+1);
        obj[length(obj)-1]:=o;
      end else begin
        for L:=0 to 7 do if (mask and (1 shl L))>0 then
                         subTrees[(L shr 2) and 1,
                                  (L shr 1) and 1,
                                   L        and 1]^.addObject(
                       box.subBox((L shr 2) and 1,
                                  (L shr 1) and 1,
                                   L        and 1 ),o,depth-1);
      end;
    end;

  PROCEDURE handDownAll;
    VAR temp:array of P_traceableObject;
        k:longint;
    begin
      setLength(temp,length(obj));
      for k:=0 to length(obj)-1 do temp[k]:=obj[k];
      setLength(obj,0);
      for k:=0 to length(temp)-1 do handDown(temp[k]);
      setLength(temp,0);
    end;

  VAR i:longint;
      j,k:shortint;
  begin
    if o^.isContainedInBox(box) then begin
      if (subTrees[0,0,0]<>nil) then begin
        handDown(o);
      end else begin
        i:=length(obj);
        setLength(obj,i+1);
        obj[i]:=o;
        if (length(obj)>=maxObjectsPerOctreeNode) and (depth>0) then begin
          for i:=0 to 1 do for j:=0 to 1 do for k:=0 to 1 do new(subTrees[i,j,k],create);
          handDownAll;
        end;
      end;
      result:=true;
    end else result:=false;
  end;

FUNCTION T_octree.rayHitsObjectInTree(CONST lowerHitTime,upperHitTime:T_Vec3; VAR ray:T_ray; OUT hitDescription:T_hitDescription):boolean;
  VAR subL,subU,midplaneTime:T_Vec3;
      newHit:T_hitDescription;
      i,j,k:shortint;
      sa:longint;
      step:T_treeSteps;
  begin

    //leaf:
    result:=false;
    hitDescription.hitTime:=1E20;
    //sb:=0;
    for sa:=0 to length(obj)-1 do if obj[sa]^.rayHits(ray,hitDescription.hitTime,newHit) then begin
      result:=true;
      hitDescription:=newHit;
    end;

    if subTrees[0,0,0]<>nil then begin
      //midplane hit times:
      midplaneTime:=0.5*(lowerHitTime+upperHitTime);
      step:=ray.getSteps(lowerHitTime,midplaneTime,upperHitTime,i,j,k);
      //find first sub-node:
      sa:=0;
      while (sa<=8) and not((i in [0,1]) and (j in [0,1]) and (k in [0,1])) do begin
        with step[sa] do begin inc(i,di); inc(j,dj); inc(k,dk); end; //next node
        inc(sa);
      end;
      //iterate over nodes until a hit occurs or the ray exits
      while  (i in [0,1]) and (j in [0,1]) and (k in [0,1]) do begin
        //if subTrees[i,j,k]^.subTrees[0,0,0]<>nil then begin
          if i=0 then begin subL[0]:=lowerHitTime[0]; subU[0]:=midPlaneTime[0]; end
                 else begin subL[0]:=midPlaneTime[0]; subU[0]:=upperHitTime[0]; end;
          if j=0 then begin subL[1]:=lowerHitTime[1]; subU[1]:=midPlaneTime[1]; end
                 else begin subL[1]:=midPlaneTime[1]; subU[1]:=upperHitTime[1]; end;
          if k=0 then begin subL[2]:=lowerHitTime[2]; subU[2]:=midPlaneTime[2]; end
                 else begin subL[2]:=midPlaneTime[2]; subU[2]:=upperHitTime[2]; end;
        //end;
        if subTrees[i,j,k]^.rayHitsObjectInTree(subL,subU,ray,newHit) then result:=true;
        if newHit.hitTime<hitDescription.hitTime then hitDescription:=newHit;
        with step[sa] do begin
          inc(i,di); inc(j,dj); inc(k,dk);
          if result and (t>hitDescription.hitTime) then exit(result);
        end; //next node
        inc(sa);
      end;
    end;
  end;

FUNCTION T_octree.rayHitsObjectInTreeInaccurate(VAR lowerHitTime,upperHitTime:T_Vec3; CONST ray:T_ray; maxHitTime:double):boolean;
  VAR subL,subU,midplaneTime:T_Vec3;
      i,j,k:shortint;
      sa:longint;
      step:T_treeSteps;
  begin

    //leaf:
    result:=false;
    //sb:=0;
    for sa:=0 to length(obj)-1 do if obj[sa]^.rayHitsInaccurate(ray,maxHitTime) then begin
      exit(true);
    end;

    if subTrees[0,0,0]<>nil then begin
      //midplane hit times:
      midplaneTime:=0.5*(lowerHitTime+upperHitTime);
      step:=ray.getSteps(lowerHitTime,midplaneTime,upperHitTime,i,j,k);

      //find first sub-node:
      sa:=0;
      while (sa<=8) and not((i in [0,1]) and (j in [0,1]) and (k in [0,1])) do begin
        with step[sa] do begin inc(i,di); inc(j,dj); inc(k,dk); end; //next node
        inc(sa);
      end;
      //iterate over nodes until a hit occurs or the ray exits
      while  (i in [0,1]) and (j in [0,1]) and (k in [0,1]) do begin
        //if subTrees[i,j,k]^.subTrees[0,0,0]<>nil then begin
          if i=0 then begin subL[0]:=lowerHitTime[0]; subU[0]:=midPlaneTime[0]; end
                 else begin subL[0]:=midPlaneTime[0]; subU[0]:=upperHitTime[0]; end;
          if j=0 then begin subL[1]:=lowerHitTime[1]; subU[1]:=midPlaneTime[1]; end
                 else begin subL[1]:=midPlaneTime[1]; subU[1]:=upperHitTime[1]; end;
          if k=0 then begin subL[2]:=lowerHitTime[2]; subU[2]:=midPlaneTime[2]; end
                 else begin subL[2]:=midPlaneTime[2]; subU[2]:=upperHitTime[2]; end;
        //end;
        if subTrees[i,j,k]^.rayHitsObjectInTreeInaccurate(subL,subU,ray,maxHitTime) then exit(true);
        with step[sa] do begin
          inc(i,di); inc(j,dj); inc(k,dk); if t>maxHitTime then exit(false);
        end; //next node
        inc(sa);
      end;
    end;
  end;

FUNCTION T_octree.containsObject(CONST o:P_traceableObject):boolean;
  VAR k:longint;
  begin
    k:=0;
    while (k<length(obj)) and (obj[k]<>o) do inc(k);
    result:=(k<length(obj)) and (length(obj)>0);
  end;

PROCEDURE T_octree.removeObject(CONST o:P_traceableObject);
  VAR k:longint;
  begin
    k:=0;
    while (k<length(obj)) and (obj[k]<>o) do inc(k);
    if k<length(obj) then begin
      obj[k]:=obj[length(obj)-1];
      setLength(obj,length(obj)-1);
    end;
  end;

FUNCTION T_octree.redistributeObjects:longint;
  VAR i:byte;
      k:longint;
      next:P_traceableObject;
  begin
    result:=0;
    if subTrees[0,0,0]<>nil then begin
      //bottom op recursion:
      for i:=0 to 7 do inc(result,subTrees[(i shr 2) and 1,(i shr 1) and 1,i and 1]^.redistributeObjects);
      //pull objects contained in all (!) children to this node:---------------------------------------------------
      k:=0;
      while k<length(subtrees[0,0,0]^.obj) do begin
        next:=subtrees[0,0,0]^.obj[k];
        i:=1; while (i<8) and (subTrees[(i shr 2) and 1,(i shr 1) and 1,i and 1]^.containsObject(next)) do inc(i);
        if i=8 then begin
          inc(result);
          setLength(obj,length(obj)+1);
          obj[length(obj)-1]:=next;
          for i:=0 to 7 do subTrees[(i shr 2) and 1,(i shr 1) and 1,i and 1]^.removeObject(next);
        end else inc(k);
      end;
      //---------------------------------------------------:pull objects contained in all (!) children to this node
      //if all children are empty, remove them:--------------------------------------------------------------------
      i:=0;
      while (i<8) and (length(subTrees[(i shr 2) and 1,(i shr 1) and 1,i and 1]^.obj)=0)
                         and (subTrees[(i shr 2) and 1,(i shr 1) and 1,i and 1]^.subTrees[0,0,0]=nil) do inc(i);
      if i=8 then for i:=0 to 7 do begin
        dispose(subTrees[(i shr 2) and 1,(i shr 1) and 1,i and 1],destroy);
        subTrees[(i shr 2) and 1,(i shr 1) and 1,i and 1]:=nil;
      end;
      //--------------------------------------------------------------------:if all children are empty, remove them
    end;
  end;


CONSTRUCTOR T_octreeRoot.create;
  begin
    rayCount:=0;
    box.createQuick;
    maxDepth:=0;
    new(tree,create);
    basePlane.present:=false;
    fog.present:=false;
  end;

DESTRUCTOR T_octreeRoot.destroy;
  VAR i:longint;
  begin
    dispose(tree,destroy);
    for i:=0 to length(allObjects)-1 do dispose(allObjects[i],destroy);
    setLength(allObjects,0);
    for i:=0 to length(allNodes)-1 do dispose(allNodes[i],destroy);
    setLength(allNodes,0);
  end;

PROCEDURE T_octreeRoot.changeDepth(newDepth:byte);
  VAR i:longint;
  begin
    maxDepth:=newDepth;
    if length(allObjects)>0 then begin
      box.destroy;
      box:=allObjects[0]^.getBoundingBox;
      for i:=0 to length(allObjects)-1 do box.uniteWith(allObjects[i]^.getBoundingBox);
    end;
    box.expandToCube;
    dispose(tree,destroy);
    tree:=nil;
    new(tree,create);
    for i:=0 to length(allObjects)-1 do tree^.addObject(box,allObjects[i],maxDepth);
    tree^.redistributeObjects;
  end;
  
PROCEDURE T_octreeRoot.registerNode(node:P_node);
  begin
    setLength(allNodes,length(allNodes)+1);
    allNodes[length(allNodes)-1]:=node;
  end;

PROCEDURE T_octreeRoot.addObject(obj:P_traceableObject);
  VAR i,j,k:longint;
      oldRoot:P_octree;
      oldSize:T_Vec3;
      obb:T_boundingBox;
  begin
    obb:=obj^.getBoundingBox;
    //is (partially) outside?
    if not(obb.isInside(box)) then begin
      if tree^.subTrees[0,0,0]=nil then begin
        //single-node tree: just adapt the corners to include new object:
        if length(tree^.obj)=0 then box:=obb //special case: very first triangle
                               else box.uniteWith(obb);
        tree^.addObject(box,obj,maxDepth);
        setLength(allObjects,length(allObjects)+1);
        allObjects[length(allObjects)-1]:=obj;
      end else begin
        //create super-node and shift origin:
        if         (obb.lower[0]<box.lower[0]) and not(obb.upper[0]>box.upper[0]) then i:=1
        else if not(obb.lower[0]<box.lower[0]) and    (obb.upper[0]>box.upper[0]) then i:=0
        else                                                                           i:=random(2);
        if         (obb.lower[1]<box.lower[1]) and not(obb.upper[1]>box.upper[1]) then j:=1
        else if not(obb.lower[1]<box.lower[1]) and    (obb.upper[1]>box.upper[1]) then j:=0
        else                                                                           j:=random(2);
        if         (obb.lower[2]<box.lower[2]) and not(obb.upper[2]>box.upper[2]) then k:=1
        else if not(obb.lower[2]<box.lower[2]) and    (obb.upper[2]>box.upper[2]) then k:=0
        else                                                                           k:=random(2);
        oldRoot:=tree;
        tree:=nil;
        new(tree,createSuperNode(oldRoot,i,j,k));
        oldSize:=box.upper-box.lower;
        box.lower:=box.lower-newVector(   i *oldSize[0],   j *oldSize[1],   k *oldSize[2]);
        box.upper:=box.upper+newVector((1-i)*oldSize[0],(1-j)*oldSize[1],(1-k)*oldSize[2]);
        addObject(obj);
      end;
    end else begin
      tree^.addObject(box,obj,maxDepth);
      setLength(allObjects,length(allObjects)+1);
      allObjects[length(allObjects)-1]:=obj;
    end;
  end;

PROCEDURE T_octreeRoot.addFlatTriangle(a,b,c:T_Vec3; material:P_material);
  VAR tri:P_flatTriangle;
  begin
    new(tri,create(a,b,c,material));
    addObject(tri);
  end;

PROCEDURE T_octreeRoot.initialize(calc:FT_calcNodeCallback; u0,u1:double; uSteps:longint; v0,v1:double; vSteps:longint; material:P_Material);
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
        box.uniteWith(L[iu][iv].node^.position);
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

PROCEDURE T_octreeRoot.initialize(calc:FT_calcNodeCallback; u0,u1,v0,v1:double; nodeCount,fixedUNodes,fixedVNodes:longint; material:P_Material);
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
  
PROCEDURE T_octreeRoot.initialize(calc:FT_calcNodeCallback; u0,u1,v0,v1:double; nodeCount:longint; material:P_Material);
  begin initialize(calc,u0,u1,v0,v1,nodeCount,-1,-1,material); end;

PROCEDURE T_octreeRoot.initialize(calc:FT_calcNodeCallback; VAR view:T_view; avgWorldY:double; Steps:longint; material:P_Material);
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
        box.uniteWith(L[iu][iv].node^.position);
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
  
PROCEDURE T_octreeRoot.initializeLocRef(calc:FT_calcNodeCallback; u0,u1:double; iu0:longint; v0,v1:double; iv0:longint; splitThreshold:double; material:P_Material);
  VAR g:T_graph;
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
  
PROCEDURE T_octreeRoot.initializeFacets(calc:FT_calcNodeCallback; u0,u1:double; iu0:longint; v0,v1:double; iv0:longint; splitThreshold:double; material:P_Material); 
  VAR g:T_graph;
      i,i0:longint;
      //sphere:P_sphere;
      mat:array[0..9] of P_material;
  begin
    for i:=0 to 9 do new(mat[i],create(white*(i+1)*0.1));
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

FUNCTION T_octreeRoot.rayHitsObjectInTree(VAR ray:T_ray; OUT hitDescription:T_hitDescription; CONST inaccurate:boolean; CONST tMax:double):boolean;
  VAR lowerTime,upperTime:T_Vec3;
      hitMaterial:P_material;
      pseudoHitTime:double;
      
  PROCEDURE prepareLowerAndUpperTimes; inline;
    begin
      if abs(ray.direction[0])>1E-6 then begin
        lowerTime[0]:=(box.lower[0]-ray.start[0])/ray.direction[0]; 
        upperTime[0]:=(box.upper[0]-ray.start[0])/ray.direction[0];
      end else begin      
        lowerTime[0]:=-1E20*sign(ray.direction[0]);
        upperTime[0]:= 1E20*sign(ray.direction[0]);
      end;
      if abs(ray.direction[1])>1E-6 then begin
        lowerTime[1]:=(box.lower[1]-ray.start[1])/ray.direction[1];
        upperTime[1]:=(box.upper[1]-ray.start[1])/ray.direction[1];
      end else begin  
        lowerTime[1]:=-1E20*sign(ray.direction[1]);
        upperTime[1]:= 1E20*sign(ray.direction[1]);
      end;  
      if abs(ray.direction[2])>1E-6 then begin
        lowerTime[2]:=(box.lower[2]-ray.start[2])/ray.direction[2];
        upperTime[2]:=(box.upper[2]-ray.start[2])/ray.direction[2];
      end else begin
        lowerTime[2]:=-1E20*sign(ray.direction[2]);
        upperTime[2]:= 1E20*sign(ray.direction[2]);
      end;
    end;    
    
  PROCEDURE tryPlaneHit; inline;
    VAR planeHitTime:double;
    begin
      if basePlane.present and (abs(ray.direction[1])>1E-6) then begin
        planeHitTime:=(basePlane.yPos-ray.start[1])/ray.direction[1];        
        if (planeHitTime>0) and (planeHitTime<=pseudoHitTime) then begin
          hitDescription.hitTime :=planeHitTime;
          hitDescription.hitPoint:=ray.start+planeHitTime*ray.direction;
          hitDescription.hitNormal:=newVector(0,1,0);
          hitMaterial:=basePlane.material;
          pseudoHitTime:=planeHitTime;
          result:=true;
        end;
      end;        
    end;
  
//   fog:record
//      present:boolean;
//      yPos:array[0..2] of double;
//      transmittanceFunc:FT_doubleOfPosCallback;
//      material:P_material;      
//    end;

  PROCEDURE tryFogHit; inline;
    CONST STEP_SIZE=1E-2;
    VAR t0,t1,tmp,hitProp,y:double;
        fogHit:boolean=false;
        fogHitTime:double;
        f01,f12:double;
    begin
      if fog.present and (abs(ray.direction[1])>1E-6) then begin
        tmp:=1/ray.direction[1];
        t0:=(fog.yPos[0]-ray.start[1])*tmp;
        t1:=(fog.yPos[2]-ray.start[1])*tmp;
        if t1<t0 then begin tmp:=t0; t0:=t1; t1:=tmp; end;
        if t0<=0 then t0:=0;                         //way through fog starts not earlier than ray
        if t1>=pseudoHitTime then t1:=pseudoHitTime; //way through fog ends latest at hit point
        if t1>t0 then begin //if fogged interval is nonempty...
          f01:=fog.density/(1E-6+fog.yPos[1]-fog.yPos[0]);
          f12:=fog.density/(1E-6+fog.yPos[1]-fog.yPos[2]);
          fogHitTime:=t0+0.5*STEP_SIZE;          
          if fog.transmittanceFunc=nil then begin
            while (fogHitTime<t1) and not(fogHit) do begin
              fogHitTime:=fogHitTime+STEP_SIZE;
              y:=ray.start[1]+fogHitTime*ray.direction[1];
              if y>fog.yPos[1] then hitProp:=(y-fog.yPos[2])*f12
                               else hitProp:=(y-fog.yPos[0])*f01;
              fogHit:=(random<hitProp);
            end;
          end else begin
            while (tmp<t1) and not(fogHit) do begin
              tmp:=tmp+STEP_SIZE;
            end;
          end;
          if fogHit then begin
            hitDescription.hitTime:=fogHitTime;
            hitDescription.hitPoint:=ray.start+ray.direction*fogHitTime;
            hitDescription.hitNormal:=randomVecOnUnitSphere;// normed(randomVecInUnitSphere-ray.direction);            
            hitMaterial:=fog.material;
            result:=true;
          end;
        end;                
      end;    
    end;
  
  begin
    interlockedIncrement(rayCount);
    if inaccurate and (tMax>9E19) and basePlane.present and (abs(ray.direction[1])>1E-6) and ((basePlane.yPos-ray.start[1])/ray.direction[1]>1E-3) then exit(true);
    prepareLowerAndUpperTimes;
    if inaccurate then result:=tree^.rayHitsObjectInTreeInaccurate(lowerTime,upperTime,ray,tMax)
                  else result:=tree^.rayHitsObjectInTree          (lowerTime,upperTime,ray,hitDescription);
    if result and inaccurate then exit(true);
    if result 
      then begin pseudoHitTime:=hitDescription.hitTime-1E-6; hitMaterial:=hitDescription.hitObject^.material; end
      else begin pseudoHitTime:=tMax                  -1E-6; hitMaterial:=nil;                                end;
    tryPlaneHit;              
    tryFogHit;
    if hitMaterial<>nil then hitDescription.hitMaterialPoint:=hitMaterial^.getMaterialPoint(hitDescription.hitPoint);                  
    //if result and not(inaccurate) then begin      
    //  if basePlane.present and (abs(ray.direction[1])>1E-6) then begin
    //    planeHitTime:=(basePlane.yPos-ray.start[1])/ray.direction[1];
    //    if (planeHitTime>0) and (planeHitTime<=hitDescription.hitTime-1E-6) and (planeHitTime<tMax) then begin
    //      hitDescription.hitTime :=planeHitTime;
    //      hitDescription.hitPoint:=ray.start+planeHitTime*ray.direction;
    //      hitDescription.hitNormal:=newVector(0,1,0);
    //      hitMaterial:=basePlane.material;
    //    end else begin
    //      hitMaterial:=hitDescription.hitObject^.material;
    //    end;
    //  end else begin
    //    hitMaterial:=hitDescription.hitObject^.material;
    //  end;      
    //end else if basePlane.present and (abs(ray.direction[1])>1E-6) then begin
    //  planeHitTime:=(basePlane.yPos-ray.start[1])/ray.direction[1];
    //  if (planeHitTime>0) and (planeHitTime<tMax) then begin
    //    result:=true;
    //    hitDescription.hitTime :=planeHitTime;
    //    hitDescription.hitPoint:=ray.start+planeHitTime*ray.direction;
    //    hitDescription.hitNormal:=newVector(0,1,0);
    //    hitMaterial:=basePlane.material;
    //  end;      
    //end;
    
  end;

FUNCTION T_octreeRoot.lightVisibility(CONST hit:T_hitDescription; CONST lazy:boolean; CONST light:T_pointLight; CONST undistortedRay:T_ray; VAR shadowByte:byte):T_floatColor; inline;
  VAR sray:T_Ray;
      dir:T_Vec3;
      tMax,w:double;
      hitDummy:T_hitDescription;
      instance:T_pointLightInstance;
      lightIsVisible:boolean;
  begin
    instance:=light.getInstance(hit.hitNormal);
    with light do begin
      if infiniteDist then begin
        dir:=instance.pos;
        w:=dir*hit.hitNormal;
        sray.createLightScan(hit.hitPoint+dir*1E-3,dir,white,1E-3,lazy);
        if not((w<=0) or rayHitsObjectInTree(sray,hitDummy,true,1E20))
          then begin
                 result:=hit.hitMaterialPoint.getColorAtPixel(hit.hitPoint,hit.hitNormal,instance);
                 lightIsVisible:=true;
                 shadowByte:=shadowByte or SHADOWMASK_LIGHT;
               end
          else begin 
                 result:=black;
                 lightIsVisible:=false;
                 shadowByte:=shadowByte or SHADOWMASK_SHADOW;
               end;
      end else begin
        dir:=instance.pos-hit.hitPoint;
        tMax:=norm(dir);
        dir:=dir*(1/tMax);
        w:=dir*hit.hitNormal/(tMax*tMax);
        tMax:=tMax-1E-3;
        sray.createLightScan(hit.hitPoint+dir*1E-3,dir,white,1E-3,lazy);
        if not((w<=0) or rayHitsObjectInTree(sray,hitDummy,true,tMax))
          then begin
                 result:=hit.hitMaterialPoint.getColorAtPixel(hit.hitPoint,hit.hitNormal,instance);
                 lightIsVisible:=true;
                 shadowByte:=shadowByte or SHADOWMASK_LIGHT;
               end   
          else begin
                 result:=black;
                 lightIsVisible:=false;
                 shadowByte:=shadowByte or SHADOWMASK_SHADOW;
               end;  
      end;
    end;
    if lighting.specularLights then begin
      if lightIsVisible and (hit.hitMaterialPoint.reflectionLevel>1E-5) then begin
        result:=result+
          hit.hitMaterialPoint.getReflected(
            light.getLookIntoLightIntegral(            
              undistortedRay,
              hit.hitNormal,
              hit.hitMaterialPoint.getReflectDistortion,
              1E20,
              shadowByte)); //tMax     
      end else shadowByte:=shadowByte OR SPECMASK_SHADOW;
    end;
  end;

FUNCTION T_octreeRoot.getHitColor(VAR ray:T_ray; CONST depth:byte; VAR lightMap:T_lightMap):T_floatColor;
  VAR hitDescription:T_hitDescription;
  VAR refractedRay:T_ray;
      i:longint;
      dummyByte:byte;
      
  FUNCTION ambientLight:T_floatColor; inline;
    VAR sray:T_Ray;
        dummy:T_hitDescription;
        w:double;
    begin
      sray.createLightScan(ray.start,randomVecOnUnitSphere,ray.weight,1E-3,true);
      w:=sRay.direction*hitDescription.hitNormal; 
      if (w<0) then begin
        w:=-w;
        sRay.direction:=-1*sRay.direction;
        sray.start:=sray.start+2E-3*sRay.direction;
      end;
      sray.weight:=sray.weight*w;
      if (greyLevel(lighting.ambientLight)<0.01) and (lighting.ambientFunc=nil) or rayHitsObjectInTree(sray,dummy,true,1E20)
        then result:=black
        else result:=hitDescription.hitMaterialPoint.getLocalAmbientColor(w*2,lighting.getBackground(sRay.direction));
    end;
     
  FUNCTION pathLight:T_FloatColor; inline;
    VAR sray:T_Ray;
        w:double;
    begin
      if (depth<=0) then exit(black);      
      sray.createPathTracing(hitDescription.hitPoint,randomVecOnUnitSphere,ray.weight,1E-3);
      w:=sRay.direction*hitDescription.hitNormal; 
      if (w<0) then begin        
        w:=-w;
        sRay.direction:=-1*sRay.direction;
        sray.start:=sray.start+2E-3*sRay.direction;
      end;
      sRay.weight:=ray.weight*w;
      result:=hitDescription.hitMaterialPoint.getLocalAmbientColor(w*2,getHitColor(sRay,depth-1,lightMap));
    end;
    
  FUNCTION mapLight:T_floatColor;
    VAR status:T_elementStatus;
        lme:P_lightMapElement;
        col:T_floatColor;
    begin
      lme:=lightMap.getCollectorAt(hitDescription.hitPoint,true,status);
      if status=ES_MISSING then begin
        if lighting.lightingModel = LIGHTING_MODEL_PATH_LIGHT_MAP 
          then exit(pathLight)
          else exit(ambientLight);
      end;
      if not(lme^.hasEntryForNormal(hitDescription.hitNormal,col)) or (status=ES_NEW) then begin
        fillLightMapElement(lightMap,lme^,hitDescription.hitPoint,hitDescription.hitNormal);
        lme^.hasEntryForNormal(hitDescription.hitNormal,col);
      end;
      result:=hitDescription.hitMaterialPoint.getLocalAmbientColor(1,col);
    end;    
    
  begin
    if not(rayHitsObjectInTree(ray,hitDescription,false,1E20)) then begin
      result:=lighting.getBackground(ray.direction);
      
      if (ray.state<>RAY_STATE_PATH_TRACING) 
        then result:=result+lighting.getLookIntoLight(ray,1E20);
    end else begin
  
      if (ray.state<>RAY_STATE_PATH_TRACING) 
        then result:=hitDescription.hitMaterialPoint.localGlowColor+lighting.getLookIntoLight(ray,hitDescription.hitTime)
        else result:=hitDescription.hitMaterialPoint.localGlowColor;
      refractedRay:=ray.reflectAndReturnRefracted(
        hitDescription.hitMaterialPoint,
        hitDescription.hitTime,
        hitDescription.hitNormal);
      
      case lighting.lightingModel of        
        LIGHTING_MODEL_IGNORANT : result:=result+hitDescription.hitMaterialPoint.getLocalAmbientColor(1,ignorantLightingLME.get(hitDescription.hitNormal));
        LIGHTING_MODEL_SIMPLE,
        LIGHTING_MODEL_LAZY_PATH_TRACING: result:= result+ambientLight;                
        LIGHTING_MODEL_PATH_TRACING     : result:= result+pathLight;                                
        LIGHTING_MODEL_PATH_LIGHT_MAP : if (ray.State and RAY_STATE_PATH_TRACING)=0 
                                        then result:=result+mapLight
                                        else result:=result+pathLight;
        LIGHTING_MODEL_OCCLUSION_MAP  : result:=result+mapLight;
      end;
           
      for i:=0 to length(lighting.pointLight)-1 do begin
        dummyByte:=SHADOWMASK_NONE;
        result:=result+lightVisibility(hitDescription,
          lighting.lightingModel in [LIGHTING_MODEL_NOAMB,LIGHTING_MODEL_IGNORANT,LIGHTING_MODEL_SIMPLE,LIGHTING_MODEL_LAZY_PATH_TRACING],
          lighting.pointLight[i],
          ray,
          dummyByte);
      end;           
          
      if (depth>0) and (hitDescription.hitMaterialPoint.reflectionLevel>1E-2) then begin
        ray.modifyReflected(hitDescription.hitNormal,hitDescription.hitMaterialPoint);
        result:=result+hitDescription.hitMaterialPoint.getReflected(
                 getHitColor(ray,depth-1,lightMap));
      end;
      //if refractedRay.rayLevel>1E-2 then begin
      if hitDescription.hitMaterialPoint.refractionLevel>1E-2 then begin
        refractedRay.modifyRefracted(hitDescription.hitNormal,hitDescription.hitMaterialPoint);      
        result:=result+hitDescription.hitMaterialPoint.getRefracted(
               getHitColor(refractedRay,depth,lightMap));
      end;    
    end;
  end;

PROCEDURE T_octreeRoot.getHitColor(CONST pixelX,pixelY:longint; CONST firstRun:boolean; VAR lightMap:T_lightMap; VAR colors:T_structuredHitColor);
  VAR ray,refractedRay:T_ray;
      sampleIndex:longint;
      maxSampleIndex:longint=2085;
      minSampleIndex:longint=-1;
      hitDescription:T_hitDescription;
           
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
            hitDescription,
            lighting.lightingModel<=LIGHTING_MODEL_SIMPLE,
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
      colors.pathOrAmbient.weight:=4*(sampleIndex+1);
    end;
  
  PROCEDURE calculateAmbientLight; inline;
    VAR sray:T_Ray;
        dummy:T_hitDescription;
        w:double;                
    begin
      if (lighting.ambientFunc=nil) and (greyLevel(lighting.ambientLight)<1E-2) then begin
        colors.pathOrAmbient.weight:=1;
      end else if colors.pathOrAmbient.scan then while colors.pathOrAmbient.weight<4*(sampleIndex+1) do begin
        sray.createLightScan(ray.start,randomVecOnUnitSphere,ray.weight,1E-3,true);               
        w:=sRay.direction*hitDescription.hitNormal; 
        if w<0 then begin
          w     :=-w;
          sray.direction:=-1*sray.direction;
          sray.start:=sray.start+2E-3*sray.direction;
        end;
        sray.weight:=sray.weight*w;
        if not(rayHitsObjectInTree(sray,dummy,true,1E20))
          then colors.pathOrAmbient.col:=colors.pathOrAmbient.col+hitDescription.hitMaterialPoint.getLocalAmbientColor(w,lighting.getBackground(sray.direction));
        colors.pathOrAmbient.weight:=colors.pathOrAmbient.weight+w;//*1.5;
      end;
    end;
    
  PROCEDURE calculatePathLight; inline;
    VAR sray:T_Ray;
        rayDir:T_Vec3;       
        w:double;
        recvColor:T_floatColor;
    begin      
      if colors.pathOrAmbient.scan and (reflectionDepth>0) then while colors.pathOrAmbient.weight<8*(sampleIndex+1) do begin
        rayDir:=randomVecOnUnitSphere; 
        w:=rayDir*hitDescription.hitNormal; 
        if w<0 then begin
          w     :=-w;
          rayDir:=-1*rayDir;
        end;
        sray.createPathTracing(ray.start,rayDir,w*ray.weight,1E-3);
        recvColor:=getHitColor(sRay,reflectionDepth-1,lightMap);             
        colors.pathOrAmbient.col:=colors.pathOrAmbient.col+hitDescription.hitMaterialPoint.getLocalAmbientColor(w,recvColor);        
        colors.pathOrAmbient.weight:=colors.pathOrAmbient.weight+w;
      end;
    end;

  PROCEDURE calculateMapLight; inline;
    VAR status:T_elementStatus;
        lme:P_lightMapElement;
        col:T_FloatColor;
    begin
      if colors.pathOrAmbient.scan then begin
        lme:=lightMap.getCollectorAt(hitDescription.hitPoint,true,status);
        if status=ES_MISSING then begin
          if lighting.lightingModel=LIGHTING_MODEL_PATH_LIGHT_MAP
          then calculatePathLight
          else calculateAmbientLight;
          exit;
        end;
        if not(lme^.hasEntryForNormal(hitDescription.hitNormal,col)) or (status=ES_NEW) then begin
          fillLightMapElement(lightMap,lme^,hitDescription.hitPoint,hitDescription.hitNormal);
          lme^.hasEntryForNormal(hitDescription.hitNormal,col);
        end;
        colors.pathOrAmbient.col:=colors.pathOrAmbient.col+
          hitDescription.hitMaterialPoint.getLocalAmbientColor(1,col);
        colors.pathOrAmbient.weight:=colors.pathOrAmbient.weight+1;
      end;
    end;    

  begin
    correctSampleRangeByMask(colors.antialiasingMask);
   
    for sampleIndex:=minSampleIndex to maxSampleIndex-1 do begin  
      ray:=view.getRay(pixelX+darts_delta[sampleIndex,0],pixelY+darts_delta[sampleIndex,1]);
      if rayHitsObjectInTree(ray,hitDescription,false,1E20) then begin

        colors.rest:=colors.rest+lighting.getLookIntoLight(ray,hitDescription.hitTime)
                                +hitDescription.hitMaterialPoint.localGlowColor;            
        
        refractedRay:=ray.reflectAndReturnRefracted(
          hitDescription.hitMaterialPoint,
          hitDescription.hitTime,
          hitDescription.hitNormal);
      
        calculateDirectLight;
        
        case lighting.lightingModel of          
          LIGHTING_MODEL_IGNORANT : with colors.pathOrAmbient do begin
            col:=col+hitDescription.hitMaterialPoint.getLocalAmbientColor(1,ignorantLightingLME.get(hitDescription.hitNormal));
            weight:=weight+1;
          end;
          LIGHTING_MODEL_SIMPLE         : calculateAmbientLight;        
          LIGHTING_MODEL_LAZY_PATH_TRACING,          
          LIGHTING_MODEL_PATH_TRACING   : calculatePathLight;     
          LIGHTING_MODEL_PATH_LIGHT_MAP,
          LIGHTING_MODEL_OCCLUSION_MAP  : calculateMapLight;
        end;
        
        if (reflectionDepth>0) and (ray.rayLevel>0.01) then begin
          ray.         modifyReflected(hitDescription.hitNormal,hitDescription.hitMaterialPoint);
          colors.rest:=colors.rest+hitDescription.hitMaterialPoint.getReflected(
            getHitColor(ray         ,reflectionDepth-1,lightMap));
        end;
        if (refractedRay.rayLevel>0.01) then begin
          refractedRay.modifyRefracted(hitDescription.hitNormal,hitDescription.hitMaterialPoint);
          colors.rest:=colors.rest+          
          hitDescription.hitMaterialPoint.getRefracted(
            getHitColor(refractedRay,reflectionDepth  ,lightMap));
        end;
      end else begin
        colors.rest:=colors.rest+lighting.getBackground(ray.direction)+lighting.getLookIntoLight(ray,1E20);
        addNoHitDirectAndAmbientLight;
      end;
    end;        
  end;  
  
PROCEDURE T_octreeRoot.fillLightMapElement(VAR lightMap:T_lightMap; VAR element:T_lightMapElement; CONST hitPoint,hitNormal:T_Vec3);  
  VAR i,j,k:longint;      
      samplePoint:T_Vec3;
      colorSum:T_FloatColor;
      weightSum:double;
      
  PROCEDURE scanInDirection(CONST d:T_Vec3);
    VAR w:double;
        ray:T_ray;
        dummy:T_hitDescription;
    begin
      w:=d*hitNormal;
      if w<=0 then exit;
      
      ray.createPathTracing(samplePoint,d,w*white,1E-6);
      if lighting.lightingModel=LIGHTING_MODEL_OCCLUSION_MAP then begin
        if not((greyLevel(lighting.ambientLight)<0.01) and (lighting.ambientFunc=nil) or rayHitsObjectInTree(ray,dummy,true,1E20))
          then colorSum:=colorSum+w*lighting.getBackground(d);
      end else begin
        colorSum:=colorSum+w*getHitColor(ray,reflectionDepth,lightMap);
        ray.destroy;
      end;
      weightSum:=weightSum+w;
    end;
  
  begin
    colorSum:=black;
    weightSum:=0;
    samplePoint:=element.getShiftedCenter(hitPoint,hitNormal,lightMap.centerPoint,lightMapGridSize);
    //samplePoint:=hitPoint;
    case lightMapQuality of
      0: for i:=0 to  11 do scanInDirection(FACET[i]);
      1: for i:=0 to  41 do scanInDirection(FACET[i]);
      2: for i:=0 to 162 do scanInDirection(FACET[i]);
      3: for i:=0 to length(FACET_ADJACENCY)-1 do for k:=0 to 5 do if FACET_ADJACENCY[i,k]<>65535 then begin
           j:=FACET_ADJACENCY[i,k];
           if j<i then scanInDirection(normed(facet[i]+facet[j]));
         end;
      4: for i:=0 to length(FACET_ADJACENCY)-1 do for k:=0 to 5 do if FACET_ADJACENCY[i,k]<>65535 then begin
           j:=FACET_ADJACENCY[i,k];        
           scanInDirection(normed(2*facet[i]+facet[j]));
         end;
      else for i:=0 to 11 do scanInDirection(FACET[i]);
    end;
    element.setColorForNormal(hitNormal,colorSum*(1/weightSum));
  end;
   
PROCEDURE T_octreeRoot.addBasePlane(y:double; material:P_material);
  begin
    basePlane.present:=true;
    basePlane.yPos:=y;
    basePlane.material:=material;
  end;
  
PROCEDURE T_octreeRoot.setFog(lowY,maxY,highY,density:double; material:P_material; transmittance:FT_doubleOfPosCallback);
  begin
    fog.present:=true;
    fog.yPos[0]:=lowY;
    fog.yPos[1]:=maxY;
    fog.yPos[2]:=highY;
    fog.density:=density;
    fog.material:=material;
    fog.transmittanceFunc:=transmittance;    
  end;

VAR samplingStatistics,
    currentSamplingStatistics:T_samplingStatistics;
  
FUNCTION prepareChunk(p:pointer):ptrint;
  CONST m:array[-1..6] of byte=(255,63,31,15,7,3,1,0);

  VAR chunk:T_colChunk;
      i,j,k:longint;      
      lightMapCenter:T_Vec3;
      llm:T_lightMap;           
  begin
    chunk.create;   
    chunk.initForChunk(renderImage.width,renderImage.height,plongint(p)^,length(lighting.pointLight));
    
    lightMapCenter:=tree.box.center;
    if tree.basePlane.present then lightMapCenter[1]:=tree.basePlane.yPos;   
    llm.create(lightMapGridSize,lightMapCenter);       
     
    tree.getHitColor(chunk.getPicX(0),chunk.getPicY(0),true,llm,chunk.col[0,0]);    
    for k:=0 to 6 do for i:=0 to chunk.width-1 do for j:=0 to chunk.height-1 do 
      if  ((i and m[k  ])=0) and ((j and m[k  ])=0) and 
      not(((i and m[k-1])=0) and ((j and m[k-1])=0)) then
      tree.getHitColor(chunk.getPicX(i),chunk.getPicY(j),true,llm,chunk.col[i,j]);          
    if (globalRenderTolerance>1E-3) then chunk.diffuseShadowMasks;    
    while (globalRenderTolerance>1E-3) and chunk.markAlias(globalRenderTolerance) do 
      for i:=0 to chunk.width-1 do for j:=0 to chunk.height-1 do if odd(chunk.col[i,j].antialiasingMask) then 
        tree.getHitColor(chunk.getPicX(i),chunk.getPicY(j),false,llm,chunk.col[i,j]);

    mergeSamplingStatistics(samplingStatistics       ,chunk.getSamplingStatistics);    
    mergeSamplingStatistics(currentSamplingStatistics,chunk.getSamplingStatistics);    

    llm.destroy;
    chunk.copyTo(renderImage);
    chunk.destroy;
    result:=0;
  end; 
 
PROCEDURE calculateImage(CONST xRes,yRes:longint; CONST repairMode:boolean; CONST filename:string);
  CONST tenMinutes=10/(24*60);
  VAR timeOfLastDump:double;
      timeOfLastProgressOutput:double;
      lastProgressOutput:double;
      progTime:array[0..31] of record t,p:double; end;
      startOfCalculation:double;

  FUNCTION dumpName:string; begin result:=changeFileExt(paramstr(0),'.dump.vraw'); end;  
      
  PROCEDURE initProgress(CONST initialProg:double);
    VAR i:longint;
    begin
      if initialProg>=0.001 then writeln('@',initialProg*100:0:2,'%');
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

      if now-timeOfLastDump>1/(24*60) then begin
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

  VAR it:longint;
      pendingChunks:T_pendingList;
      chunkCount:longint;
      chunksDone:longint=0;
      anyStarted:boolean;
      sleepTime:longint=0;

  begin
    
   
    if lighting.lightingModel=LIGHTING_MODEL_IGNORANT then ignorantLightingLME:=lighting.getGlobalLME;
    if fileExists(dumpName) then begin
      write('DUMP FOUND ');
      renderImage.create(dumpName);
      if (renderImage.width<>xRes) or (renderImage.height<>yRes) then begin
        writeln('AND REJECTED DUE TO WRONG RESOLUTION');
        renderImage.resizeDat(xRes,yRes);
        markChunksAsPending(renderImage);
      end else write('AND ACCEPTED. RESUMING CALCULATION ');
    end else begin      
      renderImage.create(xRes,yRes);
      markChunksAsPending(renderImage);
    end;    
    chunkCount:=chunksInMap(xRes,yRes);
    if repairMode then pendingChunks:=getPendingListForRepair(renderImage)
                  else pendingChunks:=getPendingList         (renderImage);
    chunksDone:=chunkCount-length(pendingChunks);
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
    
    if uppercase(extractFileExt(filename))<>'.VRAW' then begin
      writeln('postprocessing');
      shineImage(renderImage);
      colorManipulate(fk_project,0,0,0,renderImage);
    end;

    renderImage.saveSizeLimitedJpg(filename);
    renderImage.destroy;
    writeln(filename,' created in ',mytimeToStr(now-startOfCalculation));
    if fileExists(dumpName) and not(keepDump) then begin
      writeln('deleting DUMP');
      deleteFile(dumpName);
    end;
  end;

PROCEDURE calculateImage;
  VAR xRes       :longint=1366;
      yRes       :longint=768;
      filename   :string;
      repairMode :boolean=false;

  CONST cmdList:array [0..9] of T_commandAbstraction=(
    (isFile:true;  leadingSign:' '; cmdString:'';      paramCount: 0),  //0 file (for output)
    (isFile:false; leadingSign:'-'; cmdString:'';      paramCount: 2),  //1 resolution
    (isFile:false; leadingSign:'-'; cmdString:'t';     paramCount: 1),  //2 tolerance
    (isFile:false; leadingSign:'-'; cmdString:'r';     paramCount: 1),  //3 reflectionDepth
    (isFile:false; leadingSign:'-'; cmdString:'c';     paramCount: 1),  //4 number of CPUs
    (isFile:false; leadingSign:'-'; cmdString:'keepDump'; paramCount: 0),
    (isFile:false; leadingSign:'-'; cmdString:'lm'; paramCount: 1),     //6 lighting model   
    (isFile:false; leadingSign:'-'; cmdString:'lg'; paramCount: 1),     //7 light map grid size
    (isFile:false; leadingSign:'-'; cmdString:'lq'; paramCount: 1),     //8 light map quality (0..2)
    (isFile:false; leadingSign:'-'; cmdString:'repair'; paramCount: 0));    
  
  
  
  VAR i:longint;
      ep:T_extendedParameter;
      cmdLine:ansistring;
      lastCall:textfile;
  begin
    cmdLine:=extractFileName(paramstr(0))+' ';

    filename:=changeFileExt(paramstr(0),'.jpg');
    for i:=1 to paramcount do begin
      cmdLine:=cmdLine+paramstr(i)+' ';
      ep:=extendedParam(i);
      case byte(matchingCmdIndex(ep,cmdList)) of
        0: filename:=ep.cmdString;
        1: begin xres:=ep.intParam[0]; yres:=ep.intParam[1]; view.changeResolution(xRes,yRes); end;
        2: globalRenderTolerance:=ep.floatParam[0];
        3: reflectionDepth:=ep.intParam[0];
        4: numberOfCPUs:=ep.intParam[0];
        5: keepDump:=true;
        6: lighting.lightingModel:=ep.intParam[0];
        7: lightMapGridSize:=ep.floatParam[0];
        8: lightMapQuality:=ep.intParam[0] and 255;
        9: repairMode:=true;
      end;
    end;
    //for resuming:------------------------------------------------
    assign(lastCall,changeFileExt(paramstr(0),'.lastCall.bat'));
    rewrite(lastCall);
    writeln(lastCall,cmdLine);
    close(lastCall);
    //------------------------------------------------:for resuming
    writeln('output file          (#): ',filename);
    writeln('output resolution (-#x#): ',xRes,'x',yres);
    if globalRenderTolerance<=1E-3 
    then writeln('render tolerance   (-t#): 0 [PREVIEW]')
    else writeln('render tolerance   (-t#): ',globalRenderTolerance:0:3);
    writeln('reflection depth   (-r#): ',reflectionDepth);
    writeln('number of threads  (-c#): ',numberOfCPUs);
    writeln('             (-keepDump): ',keepDump);
    case lighting.lightingModel of      
      LIGHTING_MODEL_NOAMB                : writeln('Lighting model    (-lm#): ',lighting.lightingModel,' - SIMPLE (NO AMBIENT)');
      LIGHTING_MODEL_IGNORANT             : writeln('Lighting model    (-lm#): ',lighting.lightingModel,' - SIMPLE (NO AMBIENT OCCLUSION)');
      LIGHTING_MODEL_SIMPLE               : writeln('Lighting model    (-lm#): ',lighting.lightingModel,' - SIMPLE (AMBIENT OCCLUSION)');
      LIGHTING_MODEL_LAZY_PATH_TRACING    : writeln('Lighting model    (-lm#): ',lighting.lightingModel,' - LAZY PATH TRACING ');    
      LIGHTING_MODEL_PATH_TRACING         : writeln('Lighting model    (-lm#): ',lighting.lightingModel,' - PATH TRACING');    
      LIGHTING_MODEL_OCCLUSION_MAP        : writeln('Lighting model    (-lm#): ',lighting.lightingModel,' - OCCLUSION MAP');
      LIGHTING_MODEL_PATH_LIGHT_MAP       : writeln('Lighting model    (-lm#): ',lighting.lightingModel,' - PATH TRACING MAP');
      else                                  writeln('Lighting model    (-lm#): ',lighting.lightingModel,' --unknown--');    
    end;    
    writeln('light grid size   (-lg#): ',lightMapGridSize:0:5);
    writeln('light map quality (-lq#): ',lightMapQuality);    
    writeln;
    writeln('Geometry: ',length(tree.allObjects),' primitives');
    writeln('      in: ',tree.box.lower[0]:10:5,'..',tree.box.upper[0]:10:5);
    writeln('          ',tree.box.lower[1]:10:5,'..',tree.box.upper[1]:10:5);
    writeln('          ',tree.box.lower[2]:10:5,'..',tree.box.upper[2]:10:5);
    writeln;
    writeln('Lighting: ambient  ',lighting.ambientLight[0]:0:3,',',
                                  lighting.ambientLight[1]:0:3,',',
                                  lighting.ambientLight[2]:0:3);
    writeln('          points   ',length(lighting.pointLight));
    writeln('          backfunc ',(lighting.ambientFunc<>nil));
    if repairMode then begin
      writeln;
      writeln('------------------------------------------------');
      writeln('-----------  RUNNING IN REPAIR MODE  -----------');
      writeln('------------------------------------------------');      
    end;
    calculateImage(xRes,yRes,repairMode,filename);
  end;

CONSTRUCTOR I_traceableObject.init(mat:P_material);
  begin
    material:=mat;
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
    invDet:=(ray.direction[0]*by[1]*bz[2]
            +by[0]*bz[1]*ray.direction[2]
            +bz[0]*ray.direction[1]*by[2]
            -ray.direction[0]*bz[1]*by[2]
            -by[0]*ray.direction[1]*bz[2]
            -bz[0]*by[1]*ray.direction[2]);
    if (abs(invDet)>1E-20) then begin
      invDet:=1/invDet;
      a:=node[0]^.position-ray.start;
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
          result:=true;
          hitTime :=x[0];
          hitPoint:=ray.start+ray.direction*hitTime;
          hitNormal:=normed((1-x[1]-x[2])*node[0]^.normal+
                               x[1]      *node[1]^.normal+
                                    x[2] *node[2]^.normal);
          hitObject:=@self;
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
    invDet:=(ray.direction[0]*by[1]*bz[2]
            +by[0]*bz[1]*ray.direction[2]
            +bz[0]*ray.direction[1]*by[2]
            -ray.direction[0]*bz[1]*by[2]
            -by[0]*ray.direction[1]*bz[2]
            -bz[0]*by[1]*ray.direction[2]);
    if (abs(invDet)>1E-20) then begin
      invDet:=1/invDet;
      a:=node[0]^.position-ray.start;
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
    invDet:=(ray.direction[0]*by[1]*bz[2]
            +by[0]*bz[1]*ray.direction[2]
            +bz[0]*ray.direction[1]*by[2]
            -ray.direction[0]*bz[1]*by[2]
            -by[0]*ray.direction[1]*bz[2]
            -bz[0]*by[1]*ray.direction[2]);
    if (abs(invDet)>1E-6) then begin
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
          hitObject:=@self;
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
    invDet:=(ray.direction[0]*by[1]*bz[2]
            +by[0]*bz[1]*ray.direction[2]
            +bz[0]*ray.direction[1]*by[2]
            -ray.direction[0]*bz[1]*by[2]
            -by[0]*ray.direction[1]*bz[2]
            -bz[0]*by[1]*ray.direction[2]);
    if (abs(invDet)>1E-6) then begin
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

CONSTRUCTOR T_axisParallelQuad.create(c1,c2:T_Vec3; mat:P_material);
  begin
    init(mat);
    qbox.create(c1,c2);
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
    if abs(ray.direction[0])>1E-6 then begin
      invDir:=1/ray.direction[0];
      trans[0].t:=(qBox.lower[0]-ray.start[0])*invDir;
      trans[1].t:=(qBox.upper[0]-ray.start[0])*invDir;
      if (trans[0].t<0) and (trans[1].t<0) then exit(false);
    end else exit(false);
    if abs(ray.direction[1])>1E-6 then begin
      invDir:=1/ray.direction[1];
      trans[2].t:=(qBox.lower[1]-ray.start[1])*invDir;
      trans[3].t:=(qBox.upper[1]-ray.start[1])*invDir;
      if (trans[2].t<0) and (trans[3].t<0) then exit(false);
    end else exit(false);
    if abs(ray.direction[2])>1E-6 then begin
      invDir:=1/ray.direction[2];
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
            'x':hitNormal:=newVector(1,0,0);
            'y':hitNormal:=newVector(0,1,0);
            'z':hitNormal:=newVector(0,0,1);
          end;
          hitObject:=@self;
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
    if abs(ray.direction[0])>1E-6 then begin
      invDir:=1/ray.direction[0];
      trans[0].t:=(qBox.lower[0]-ray.start[0])*invDir;
      trans[1].t:=(qBox.upper[0]-ray.start[0])*invDir;
      if (trans[0].t<0) and (trans[1].t<0) then exit(false);
    end else exit(false);
    if abs(ray.direction[1])>1E-6 then begin
      invDir:=1/ray.direction[1];
      trans[2].t:=(qBox.lower[1]-ray.start[1])*invDir;
      trans[3].t:=(qBox.upper[1]-ray.start[1])*invDir;
      if (trans[2].t<0) and (trans[3].t<0) then exit(false);
    end else exit(false);
    if abs(ray.direction[2])>1E-6 then begin
      invDir:=1/ray.direction[2];
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
    result:=qbox;
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
    inRoot:=system.sqr(ray.direction*relCenter)+(radius*radius-sqnorm(relCenter));
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
      hitObject:=@self;
    end;
  end;

FUNCTION T_sphere.rayHitsInaccurate(CONST ray:T_ray; CONST maxHitTime:double):boolean;
  VAR inRoot,t0,t1,dc:double;
      relCenter:T_Vec3;
  begin
    relCenter:=center-ray.start; //relative center
    inRoot:=system.sqr(ray.direction*relCenter)+(radius*radius-sqnorm(relCenter));
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

INITIALIZATION
  samplingStatistics:=zeroSamplingStatistics;
  currentSamplingStatistics:=zeroSamplingStatistics;
  
end.
