unit calc_lib;

{$mode objfpc}{$H+}
{$notes off}

//{$R calc.rc}

interface

function add(a,b:integer): integer;
function sub(a,b:integer): integer;

implementation

uses __common, __resource, calc_res;

type
  TAdd2 = function(a: integer; b: integer): integer;

var
  handle: THandle;

function add(a,b:integer): integer;
var
  Add2: TAdd2;
begin
   //result:=a+b
   Add2 := TAdd2(GetProcAddress(Handle, 'add2'));
   Result := Add2(a, b);
end;

function sub(a,b:integer): integer;
begin
   result:=a-b
end;

initialization
begin
  handle := __resource.TResourceSystem.LoadDllByName('UNIT_CALC_LIB_CALC_DLL');
end;

finalization
begin
  FreeLibrary(handle);
end;

end.
