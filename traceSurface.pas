PROGRAM traceSurface;
USES complex,mypics,raytrace,linAlg3d,sysutils;
VAR ray:T_Ray;
    mat0,mat1:T_material;

CONST xRes=1366;
      yRes=768;

FUNCTION moebius(x,y:double):T_Vec3;
  begin
    result[0]:=(1+y*system.cos(0.5*x))*system.cos(x);
    result[1]:=   y*system.sin(0.5*x);
    result[2]:=(1+y*system.cos(0.5*x))*system.sin(x);
  end;

begin
  randomize;
  view.create(xRes,yRes,newVector(4,2,2),newVector(0,0,0),45);
  mat0.create(0.7,0.7,0.7);
  mat0.reflectiveness:=newVector(0.5,0.5,0.5);
  mat0.reflectDistortion:=0.1;

  mat1.create(1,0.5,0);
  mat1.reflectiveness:=newVector(0.2,0.1,0.0);
  mat1.specularColor:=newVector(1,1,1);

  lighting.create;
  lighting.ambientLight:=white*0.5;
  lighting.addPointLight(normed(newVector(2,4,-2)),white*0.5);

  tree.create;
  tree.initialize(@moebius,0,2*pi,100,-0.2,0.2,20,@mat1);
  tree.addBasePlane(-0.2,@mat0);
end.
