UNIT rayTraceUtil;
INTERFACE
USES math,linalg3d;

CONST C_TetrahedronNodes:array[0..3] of T_Vec3=(
        ( 5.77350269189626E-001, 5.77350269189626E-001, 5.77350269189626E-001),
        (-5.77350269189626E-001,-5.77350269189626E-001, 5.77350269189626E-001),
        (-5.77350269189626E-001, 5.77350269189626E-001,-5.77350269189626E-001),
        ( 5.77350269189626E-001,-5.77350269189626E-001,-5.77350269189626E-001));
      C_tetrahedronFaces:array[0..3,0..2] of byte=((2,1,0),(3,1,2),(0,3,2),(0,1,3));
      C_CubeNodes:array[0..7] of T_Vec3=(
        ( 5.77350269189626E-001, 5.77350269189626E-001, 5.77350269189626E-001),
        (-5.77350269189626E-001, 5.77350269189626E-001, 5.77350269189626E-001),
        ( 5.77350269189626E-001,-5.77350269189626E-001, 5.77350269189626E-001),
        (-5.77350269189626E-001,-5.77350269189626E-001, 5.77350269189626E-001),
        ( 5.77350269189626E-001, 5.77350269189626E-001,-5.77350269189626E-001),
        (-5.77350269189626E-001, 5.77350269189626E-001,-5.77350269189626E-001),
        ( 5.77350269189626E-001,-5.77350269189626E-001,-5.77350269189626E-001),
        (-5.77350269189626E-001,-5.77350269189626E-001,-5.77350269189626E-001));
      C_CubeFaces:array[0..5,0..3] of byte=((0,1,3,2),(6,7,5,4),(0,2,6,4),(5,7,3,1),(0,4,5,1),(3,7,6,2));
      C_OctahedronNodes:array[0..5] of T_Vec3=(
        ( 0.00000000000000E+000, 0.00000000000000E+000, 1.00000000000000E+000),
        ( 0.00000000000000E+000, 0.00000000000000E+000,-1.00000000000000E+000),
        ( 1.00000000000000E+000, 0.00000000000000E+000, 0.00000000000000E+000),
        (-1.00000000000000E+000, 0.00000000000000E+000, 0.00000000000000E+000),
        ( 0.00000000000000E+000, 1.00000000000000E+000, 0.00000000000000E+000),
        ( 0.00000000000000E+000,-1.00000000000000E+000, 0.00000000000000E+000));
      C_OctahedronFaces:array[0..7,0..2] of byte=((2,4,0),(0,4,3),(0,5,2),(3,5,0),(1,4,2),(3,4,1),(2,5,1),(1,5,3));
      C_DodecahedronNodes:array[0..19] of T_Vec3=(
        ( 5.77350269189626E-001, 5.77350269189626E-001, 5.77350269189626E-001),
        (-5.77350269189626E-001, 5.77350269189626E-001, 5.77350269189626E-001),
        ( 5.77350269189626E-001,-5.77350269189626E-001, 5.77350269189626E-001),
        (-5.77350269189626E-001,-5.77350269189626E-001, 5.77350269189626E-001),
        ( 5.77350269189626E-001, 5.77350269189626E-001,-5.77350269189626E-001),
        (-5.77350269189626E-001, 5.77350269189626E-001,-5.77350269189626E-001),
        ( 5.77350269189626E-001,-5.77350269189626E-001,-5.77350269189626E-001),
        (-5.77350269189626E-001,-5.77350269189626E-001,-5.77350269189626E-001),
        ( 0.00000000000000E+000, 3.56822089773090E-001, 9.34172358962716E-001),
        ( 0.00000000000000E+000,-3.56822089773090E-001, 9.34172358962716E-001),
        ( 0.00000000000000E+000, 3.56822089773090E-001,-9.34172358962716E-001),
        ( 0.00000000000000E+000,-3.56822089773090E-001,-9.34172358962716E-001),
        ( 3.56822089773090E-001, 9.34172358962716E-001, 0.00000000000000E+000),
        (-3.56822089773090E-001, 9.34172358962716E-001, 0.00000000000000E+000),
        ( 3.56822089773090E-001,-9.34172358962716E-001, 0.00000000000000E+000),
        (-3.56822089773090E-001,-9.34172358962716E-001, 0.00000000000000E+000),
        ( 9.34172358962716E-001, 0.00000000000000E+000, 3.56822089773090E-001),
        (-9.34172358962716E-001, 0.00000000000000E+000, 3.56822089773090E-001),
        ( 9.34172358962716E-001, 0.00000000000000E+000,-3.56822089773090E-001),
        (-9.34172358962716E-001, 0.00000000000000E+000,-3.56822089773090E-001));
      C_DodecahedronFaces:array[0..11,0..4] of byte=((12,13,1,8,0),(0,8,9,2,16),(16,18,4,12,0),(17,3,9,8,1),(1,13,5,19,17),(2,9,3,15,14),(2,14,6,18,16),(17,19,7,15,3),(4,10,5,13,12),(18,6,11,10,4),(5,10,11,7,19),(14,15,7,11,6));
      C_IcosahedronNodes:array[0..11] of T_Vec3=(
        ( 0.00000000000000E+000, 8.50650808352040E-001, 5.25731112119134E-001),
        ( 5.25731112119134E-001, 0.00000000000000E+000, 8.50650808352040E-001),
        ( 8.50650808352040E-001, 5.25731112119134E-001, 0.00000000000000E+000),
        (-5.25731112119134E-001, 0.00000000000000E+000, 8.50650808352040E-001),
        (-8.50650808352040E-001, 5.25731112119134E-001, 0.00000000000000E+000),
        ( 0.00000000000000E+000,-8.50650808352040E-001, 5.25731112119134E-001),
        ( 8.50650808352040E-001,-5.25731112119134E-001, 0.00000000000000E+000),
        (-8.50650808352040E-001,-5.25731112119134E-001, 0.00000000000000E+000),
        ( 0.00000000000000E+000, 8.50650808352040E-001,-5.25731112119134E-001),
        ( 5.25731112119134E-001, 0.00000000000000E+000,-8.50650808352040E-001),
        (-5.25731112119134E-001, 0.00000000000000E+000,-8.50650808352040E-001),
        ( 0.00000000000000E+000,-8.50650808352040E-001,-5.25731112119134E-001));
      C_icosahedronFaces:array[0..19,0..2] of byte=((1,2,0),(0,4,3),(5,6,1),(3,7,5),(2,9,8),(8,10,4),(11,9,6),(7,10,11),(0,3,1),(1,3,5),(9,10,8),(11,10,9),(2,8,0),(0,8,4),(5,11,6),(7,11,5),(1,6,2),(4,7,3),(2,6,9),(10,7,4));

TYPE T_noiseTable=array[0..31,0..31,0..31] of single;
PROCEDURE initNoise(seed:longint);
FUNCTION perlinNoise2D(x,y:double; zIdx:longint):double;
FUNCTION perlinNoise3D(x,y,z:double):double;
IMPLEMENTATION
VAR noiseTable:T_noiseTable;

PROCEDURE initNoise(seed:longint);
  VAR i,j,k:longint;
      minVal,maxVal,v:double;
  begin
    randSeed:=seed;
    for i:=0 to 31 do for j:=0 to 31 do for k:=0 to 31 do noiseTable[i,j,k]:=random;

    minVal:= 1E20;
    maxVal:=-1E20;
    for i:=0 to 99999 do begin
      v:=perlinNoise3D(100*(random-0.5),100*(random-0.5),100*(random-0.5));
      if v<minVal then minVal:=v;
      if v>maxVal then maxVal:=v;
    end;
    for i:=0 to 31 do for j:=0 to 31 do for k:=0 to 31 do noiseTable[i,j,k]:=(noiseTable[i,j,k]-minVal)/(maxVal-minVal);
  end;

PROCEDURE calcQuadrupel(VAR t:double; OUT i0,i1,i2,i3:longint; OUT w0,w1,w2,w3:double); inline;
  begin
    i0:=floor(t); t:=t-i0;
    i3:=(i0+3) and 31;
    i2:=(i0+2) and 31;
    i1:=(i0+1) and 31;
    i0:=(i0  ) and 31;
    w0:=(t*(-0.5+(1-t*0.5)*t));
    w1:=(1+t*t*(-2.5+(3*t)*0.5));
    w2:=(t*(0.5+(2-(3*t)*0.5)*t));
    w3:=((-0.5+t*0.5)*t*t);
  end;

FUNCTION perlinNoise2D(x,y:double; zIdx:longint):double;
  FUNCTION smoothValue(tx,ty:double):double;
    VAR w0,w1,w2,w3,
        r0,r1,r2,r3:double;
        i0,i1,i2,i3,
        j0,j1,j2,j3:longint;
    begin
      calcQuadrupel(tx,i0,i1,i2,i3,w0,w1,w2,w3);
      calcQuadrupel(ty,j0,j1,j2,j3,r0,r1,r2,r3);
      result:=(noiseTable[i0,j0,zIdx]*r0+
               noiseTable[i0,j1,zIdx]*r1+
               noiseTable[i0,j2,zIdx]*r2+
               noiseTable[i0,j3,zIdx]*r3)*w0+
              (noiseTable[i1,j0,zIdx]*r0+
               noiseTable[i1,j1,zIdx]*r1+
               noiseTable[i1,j2,zIdx]*r2+
               noiseTable[i1,j3,zIdx]*r3)*w1+
              (noiseTable[i2,j0,zIdx]*r0+
               noiseTable[i2,j1,zIdx]*r1+
               noiseTable[i2,j2,zIdx]*r2+
               noiseTable[i2,j3,zIdx]*r3)*w2+
              (noiseTable[i3,j0,zIdx]*r0+
               noiseTable[i3,j1,zIdx]*r1+
               noiseTable[i3,j2,zIdx]*r2+
               noiseTable[i3,j3,zIdx]*r3)*w3;
    end;
  VAR i:longint;
      weight:double=1;
      sum:double=0;
      totWeight:double=0;
      temp:double;
  begin
    if (abs(x)<100000) and (abs(y)<100000) then begin
      for i:=0 to 10 do begin
        totWeight:=totWeight+weight;
        sum      :=sum+weight*smoothValue(x,y);
        weight:=weight*0.7;
        temp:=   x;
        x   :=  -y*1.6;
        y   :=temp*1.6;
      end;
      result:=sum/totWeight;
    end else result:=0;
  end;

FUNCTION perlinNoise3D(x,y,z:double):double;
  FUNCTION smoothValue(tx,ty,tz:double):double;
    VAR w0,w1,w2,w3,
        r0,r1,r2,r3,
        s0,s1,s2,s3:double;
        i0,i1,i2,i3,
        j0,j1,j2,j3,
        k0,k1,k2,k3:longint;
    begin
      calcQuadrupel(tx,i0,i1,i2,i3,w0,w1,w2,w3);
      calcQuadrupel(ty,j0,j1,j2,j3,r0,r1,r2,r3);
      calcQuadrupel(tz,k0,k1,k2,k3,s0,s1,s2,s3);
      result:=((noiseTable[i0,j0,k0]*r0+noiseTable[i0,j1,k0]*r1+noiseTable[i0,j2,k0]*r2+noiseTable[i0,j3,k0]*r3)*w0+
               (noiseTable[i1,j0,k0]*r0+noiseTable[i1,j1,k0]*r1+noiseTable[i1,j2,k0]*r2+noiseTable[i1,j3,k0]*r3)*w1+
               (noiseTable[i2,j0,k0]*r0+noiseTable[i2,j1,k0]*r1+noiseTable[i2,j2,k0]*r2+noiseTable[i2,j3,k0]*r3)*w2+
               (noiseTable[i3,j0,k0]*r0+noiseTable[i3,j1,k0]*r1+noiseTable[i3,j2,k0]*r2+noiseTable[i3,j3,k0]*r3)*w3)*s0+
              ((noiseTable[i0,j0,k1]*r0+noiseTable[i0,j1,k1]*r1+noiseTable[i0,j2,k1]*r2+noiseTable[i0,j3,k1]*r3)*w0+
               (noiseTable[i1,j0,k1]*r0+noiseTable[i1,j1,k1]*r1+noiseTable[i1,j2,k1]*r2+noiseTable[i1,j3,k1]*r3)*w1+
               (noiseTable[i2,j0,k1]*r0+noiseTable[i2,j1,k1]*r1+noiseTable[i2,j2,k1]*r2+noiseTable[i2,j3,k1]*r3)*w2+
               (noiseTable[i3,j0,k1]*r0+noiseTable[i3,j1,k1]*r1+noiseTable[i3,j2,k1]*r2+noiseTable[i3,j3,k1]*r3)*w3)*s1+
              ((noiseTable[i0,j0,k2]*r0+noiseTable[i0,j1,k2]*r1+noiseTable[i0,j2,k2]*r2+noiseTable[i0,j3,k2]*r3)*w0+
               (noiseTable[i1,j0,k2]*r0+noiseTable[i1,j1,k2]*r1+noiseTable[i1,j2,k2]*r2+noiseTable[i1,j3,k2]*r3)*w1+
               (noiseTable[i2,j0,k2]*r0+noiseTable[i2,j1,k2]*r1+noiseTable[i2,j2,k2]*r2+noiseTable[i2,j3,k2]*r3)*w2+
               (noiseTable[i3,j0,k2]*r0+noiseTable[i3,j1,k2]*r1+noiseTable[i3,j2,k2]*r2+noiseTable[i3,j3,k2]*r3)*w3)*s2+
              ((noiseTable[i0,j0,k3]*r0+noiseTable[i0,j1,k3]*r1+noiseTable[i0,j2,k3]*r2+noiseTable[i0,j3,k3]*r3)*w0+
               (noiseTable[i1,j0,k3]*r0+noiseTable[i1,j1,k3]*r1+noiseTable[i1,j2,k3]*r2+noiseTable[i1,j3,k3]*r3)*w1+
               (noiseTable[i2,j0,k3]*r0+noiseTable[i2,j1,k3]*r1+noiseTable[i2,j2,k3]*r2+noiseTable[i2,j3,k3]*r3)*w2+
               (noiseTable[i3,j0,k3]*r0+noiseTable[i3,j1,k3]*r1+noiseTable[i3,j2,k3]*r2+noiseTable[i3,j3,k3]*r3)*w3)*s3;
    end;
  VAR i:longint;
      weight:double=1;
      sum:double=0;
      totWeight:double=0;
      temp:double;
  begin
    if (abs(x)<100000) and (abs(y)<100000) and (abs(z)<100000) then begin
      for i:=0 to 10 do begin
        totWeight:=totWeight+weight;
        sum      :=sum+weight*smoothValue(x,y,z);
        weight:=weight*0.7;
        temp:=   x;
        x   :=  -y*1.6;
        y   :=   z*1.6;
        z   :=temp*1.6;
      end;
      result:=sum/totWeight;
    end else result:=0;
  end;


end.
