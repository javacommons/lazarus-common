unit __resource {namespace my_sample};

{$mode objfpc}{$H+}
{$notes off}

interface

type
  TResourceSystem = class
  //private
  //  length, Width: integer;

  public
    constructor Create();
    // Free で destructor が呼ばれるために override が必要
    destructor Destroy(); override;
    class function LoadDllByName(Name: string): THandle;
  end;

implementation

uses
  Classes, SysUtils, LazUTF8, __common, __win32;

constructor TResourceSystem.Create();
begin
end;

destructor TResourceSystem.Destroy();
begin
  inherited;
end;

class function TResourceSystem.LoadDllByName(Name: string): THandle;
var
  guid: string;
  //TempDir: string;
  DllPathStr: string;
  S: Classes.TResourceStream;
  F: Classes.TFileStream;
begin
  Result := 0;
  guid := __common.resource_sha256(Name);
  //TempDir := temp_dir();
  DllPathStr := temp_dir() + '\tmp.' +
    SysUtils.ExtractFileName(applicaton_fullpath()) + '.' +
    Name + '.' + guid + '.dll';
  SafeWriteLn(DllPathStr);
  //DllPathPtr := @DllPathStr[1];
  if SysUtils.FileExists(DllPathStr) then
  begin
    printf('reusing dll file: %s'#10, [DllPathStr]);
  end
  else
  begin
    if not __common.resource_to_file(Name, DllPathStr) then
      Exit;
  end;
  Result := Win32.LoadLibrary(DllPathStr);
end;

end.
