UNIT imageStashes;
INTERFACE
USES mypics;
TYPE
{Image stash; owned by a serial workflow}
F_errorFeedbackRoutine=PROCEDURE(CONST message:string) of object;
P_imageStash=^T_imageStash;
T_imageStash=object
  private
    item:array of record
           img:P_rawImage;
           id:string;
         end;
    errorRoutine:F_errorFeedbackRoutine;
  public
    CONSTRUCTOR create(CONST onError:F_errorFeedbackRoutine);
    DESTRUCTOR destroy;
  private
    FUNCTION getStashIndexForId(CONST id:string; CONST allowCreation:boolean):longint;
  public
    PROCEDURE clear;
    PROCEDURE stashImage      (CONST id:string; VAR source:T_rawImage);
    PROCEDURE unstashImage    (CONST id:string; VAR target:T_rawImage);
    FUNCTION  getStashedImage (CONST id:string): P_rawImage;
    PROCEDURE clearSingleStash(CONST id:string);
end;

IMPLEMENTATION
CONSTRUCTOR T_imageStash.create(CONST onError: F_errorFeedbackRoutine);
  begin
    errorRoutine:=onError;
    setLength(item,0);
  end;

DESTRUCTOR T_imageStash.destroy;
  begin
    clear;
  end;

FUNCTION T_imageStash.getStashIndexForId(CONST id: string; CONST allowCreation: boolean): longint;
  VAR i:longint;
  begin
    result:=-1;
    for i:=0 to length(item)-1 do if item[i].id=id then exit(i);
    if allowCreation then begin
      result:=length(item);
      setLength(item,result+1);
      item[result].id :=id;
      item[result].img:=nil;
    end;
  end;

PROCEDURE T_imageStash.clear;
  VAR i:longint;
  begin
    for i:=0 to length(item)-1 do with item[i] do begin
      if img<>nil then dispose(img,destroy);
      img:=nil;
      id:='';
    end;
    setLength(item,0);
  end;

PROCEDURE T_imageStash.stashImage(CONST id: string; VAR source: T_rawImage);
  begin
    with item[getStashIndexForId(id,true)] do begin
      if img<>nil then dispose(img,destroy);
      new(img,create(source));
    end;
  end;

PROCEDURE T_imageStash.unstashImage(CONST id: string; VAR target: T_rawImage);
  VAR i:longint;
  begin
    i:=getStashIndexForId(id,false);
    if i<0 then begin
      errorRoutine('Invalid stash  "'+id+'"');
      exit;
    end;
    with item[i] do
    if img=nil then errorRoutine('Uninitialized stash "'+id+'"')
    else target.copyFromPixMap(img^);
  end;

FUNCTION T_imageStash.getStashedImage(CONST id: string): P_rawImage;
  VAR i:longint;
  begin
    i:=getStashIndexForId(id,false);
    if i<0 then begin
      errorRoutine('Invalid stash  "'+id+'"');
      exit(nil);
    end;
    with item[i] do begin
      if img=nil then errorRoutine('Uninitialized stash "'+id+'"');
      result:=img;
    end;
  end;

PROCEDURE T_imageStash.clearSingleStash(CONST id: string);
  VAR i:longint;
  begin
    i:=getStashIndexForId(id,false);
    if (i>0) then with item[i] do if img<>nil then begin
      dispose(img,destroy);
      img:=nil;
    end;
  end;

end.

