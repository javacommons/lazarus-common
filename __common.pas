unit __common;

{$mode objfpc}{$H+}
{$notes off}

interface

uses
  Classes,
  SysUtils,
  //FileUtil,
  Variants,
  fpjson, fphttpclient, opensslsockets;

function os_bit(): uint32;
function home_dir(): string;
function config_dir(): string;
function config_json(init: string = '{}'): string;
//function get_string_from_url(url: string): string;
function download_to_file_from_url(dest: string; url: string): boolean;
function download_to_file_from_url_w_callback(dest: string; url: string;
  cb1: TNotifyEvent; cb2: TDataEvent): boolean;
procedure extract_zip(zipFileName: string; destDir: string);
function temp_dir: string;
procedure pause();
procedure pause(msg: string);
procedure SafeWriteLn();
procedure SafeWrite(msg: string);
procedure SafeWrite(msg: WideString);
procedure SafeWrite(msg: variant);
procedure SafeWriteLn(msg: variant);
procedure echo(msg: PChar; title: string = '');
procedure echo(msg: pwidechar; title: string = '');
procedure echo(msg: variant; title: string = '');
procedure echo(msg: string; title: string);
procedure echo(msg: WideString; title: string);
procedure echo(msg: TJSONData; title: string = '');
procedure printf(const fmt: string; const args: array of const);
procedure msgbox(msg: string; title: string = 'Message');
procedure msgbox(msg: WideString; title: WideString = 'Message');
function yesno(msg: string; title: string = 'Confirm'): boolean;
function yesno(msg: WideString; title: WideString = 'Confirm'): boolean;
function resource_as_memory_stream(Name: string): TMemoryStream;
function resource_sha256(Name: string): string;
function applicaton_fullpath(): string;
function resource_to_file(Name: string; fileName: string): boolean;
function load_dll_from_resource_temp(Name: string): THandle;
//function load_dll_from_resource(Name: string): THandle;
function StreamsAreIdentical(Stream1, Stream2: TStream): boolean;
function FormatByteSize(ABytes: int64): string;
procedure msleep(msec: integer);
function SecondsToTimeString(n: integer): string;
function MillisecondsToTimeString(n: integer): string;

implementation

uses
  LazUTF8
  , FileUtil
  , sha2
  , zipper
  , __win32
  , __json
  , __fs;

function os_bit(): uint32;
begin
  Result := sizeof(size_t) * 8;
end;

function home_dir(): string;
begin
  Result := GetUserDir();
end;

function config_dir(): string;
begin
  Result := GetAppConfigDir(False);
  //ForceDirectories(IncludeTrailingPathDelimiter(Result) + 'abc\xyz');
  ForceDirectories(Result);
end;

function config_json(init: string = '{}'): string;
var
  MyFile: TextFile;
  dir: string;
begin
  dir := config_dir;
  Result := IncludeTrailingPathDelimiter(dir) + 'config.json';
  if not FileExists(Result) then
  begin
    AssignFile(MyFile, Result); // ファイルを開く
    Rewrite(MyFile); // ファイルを書き込みモードで開く
    //MyString := 'Hello, world!'; // 書き込む文字列
    WriteLn(MyFile, init); // ファイルに文字列を書き込む
    CloseFile(MyFile); // ファイルを閉じる
  end;
end;

{
function get_string_from_url(url: string): string;
begin
  Result := '';
  with TFPHttpClient.Create(nil) do
    try
      AllowRedirect := True;
      Result := Get(url);
    finally
      Free;
    end;
end;
}

function download_to_file_from_url(dest: string; url: string): boolean;
var
  guid: TGuid;
  temp: string;
begin
  CreateGuid(guid);
  temp := dest + GuidToString(guid) + '.tmp';
  with TFPHttpClient.Create(nil) do
    try
      AllowRedirect := True;
      Get(url, temp);
      Result := __fs.RenameFile(temp, dest);
    finally
      Free;
    end;
end;

function download_to_file_from_url_w_callback(dest: string; url: string;
  cb1: TNotifyEvent; cb2: TDataEvent): boolean;
var
  guid: TGuid;
  temp: string;
begin
  CreateGuid(guid);
  temp := dest + GuidToString(guid) + '.tmp';
  with TFPHttpClient.Create(nil) do
    try
      OnHeaders := cb1;
      OnDataReceived := cb2;
      AllowRedirect := True;
      Get(url, temp);
      SysUtils.DeleteFile(dest);
      Result := __fs.RenameFile(temp, dest);
    finally
      Free;
    end;
end;

procedure extract_zip(zipFileName: string; destDir: string);
var
  szip: TUnZipper;
var
  guid: TGuid;
  tempDir: string;
begin
  printf('extract_zip():'#10, []);
  printf('  zipFileName: %s:'#10, [zipFileName]);
  printf('  destDir: %s:'#10, [destDir]);
  SafeWriteLn();
  SafeWrite(#27'[1A'); // moves your cursor up one line, but in the same column
  SafeWrite(#13); // brings your cursor to the beginning of the line
  SafeWrite(#27'[2K'); // erases the entire line your cursor is currently on
  printf('  Progress: 0%%'#10, []);
  CreateGuid(guid);
  tempDir := destDir + GuidToString(guid) + '.tmp';
  try
    szip := TUnZipper.Create;
    szip.FileName := Utf8ToAnsi(zipFileName);
    szip.OutputPath := Utf8ToAnsi(tempDir);
    szip.UnZipAllFiles();
    if not __fs.RenameDirectory(tempDir, destDir) then
    begin
      msgbox('extract_zip() rename failed');
    end;
  finally
    szip.Free;
  end;
  SafeWrite(#27'[1A'); // moves your cursor up one line, but in the same column
  SafeWrite(#13); // brings your cursor to the beginning of the line
  SafeWrite(#27'[2K'); // erases the entire line your cursor is currently on
  printf('  Progress: 100%%'#10, []);
  //SafeWriteLn();
end;

function temp_dir: string;
{
var
  pc: pwidechar;
begin
  pc := pwidechar(StrAlloc((MAX_PATH + 1) * 2));
  GetTempPathW(MAX_PATH, pc);
  Result := UTF16ToUTF8(WideString(pc));
  StrDispose(pc);
end;
}
begin
  Result := Win32.GetTempPath;
end;

procedure pause();
begin
  pause('Click OK to continue');
end;

procedure pause(msg: string);
begin
  Win32.MessageBox(0, msg, 'Pause');
end;

procedure SafeWrite(msg: string);
begin
  try
    Write(msg);
  except
  end;
end;

procedure SafeWrite(msg: WideString);
begin
  try
    Write(Utf16ToUtf8(msg));
  except
  end;
end;

procedure SafeWrite(msg: variant);
begin
  try
    Write(msg);
  except
  end;
end;

procedure SafeWriteLn();
begin
  try
    WriteLn();
  except
  end;
end;

procedure SafeWriteLn(msg: variant);
begin
  try
    WriteLn(msg);
  except
  end;
end;

{
procedure SafeWriteLn(msg: string);
begin
  try
    WriteLn(msg);
  except
  end;
end;

procedure SafeWriteLn(msg: widestring);
begin
  try
    WriteLn(Utf16ToUtf8(msg));
  except
  end;
end;
}

procedure echo(msg: variant; title: string);
begin
  if not title.IsEmpty() then
  begin
    SafeWrite(UTF8ToConsole(title));
    SafeWrite(': ');
  end;
  try
    WriteLn(msg);
  except
  end;
end;

procedure echo(msg: TJSONData; title: string);
begin
  echo(Format('<%s> %s', [typeof(msg), msg.FormatJSON]), title);
end;

procedure echo(msg: string; title: string);
begin
  if not title.IsEmpty() then
  begin
    SafeWrite(UTF8ToConsole(title));
    SafeWrite(': ');
  end;
  SafeWriteLn(UTF8ToConsole(msg));
end;

procedure echo(msg: WideString; title: string);
begin
  if not title.IsEmpty() then
  begin
    SafeWrite(UTF8ToConsole(title));
    SafeWrite(': ');
  end;
  SafeWriteLn(UTF8ToConsole(UTF16ToUTF8(msg)));
end;

procedure echo(msg: PChar; title: string);
var
  s: string;
begin
  s := msg;
  echo(s, title);
end;

procedure echo(msg: pwidechar; title: string);
var
  s: WideString;
begin
  s := msg;
  echo(s, title);
end;

procedure printf(const fmt: string; const args: array of const);
begin
  SafeWrite(format(fmt, args));
end;


procedure msgbox(msg: string; title: string);
begin
  Win32.MessageBox(0, msg, title);
end;


procedure msgbox(msg: WideString; title: WideString);
begin
  Win32.MessageBox(0, msg, title);
end;

function yesno(msg: string; title: string = 'Confirm'): boolean;
begin
  Result := Win32.MessageBoxYesNo(0, msg, title);
end;

function yesno(msg: WideString; title: WideString = 'Confirm'): boolean;
begin
  Result := Win32.MessageBoxYesNo(0, msg, title);
end;

function resource_as_memory_stream(Name: string): TMemoryStream;
var
  S: TResourceStream;
begin
  Result := nil;
  try
    S := Win32.CreateResourceStream(Name);
    Result := TMemoryStream.Create();
    Result.CopyFrom(S, S.Size);
    Result.Seek(0, soFromBeginning);
  finally
    S.Free;
  end;
end;

function resource_sha256(Name: string): string;
var
  ms: TMemoryStream;
  ctx: array of TSha2Context;
  digest: TSha2Digest;
begin
  try
    ms := resource_as_memory_stream(Name);
    SetLength(ctx, 1);
    Sha2Init(ctx[0], SHA2_256);
    Sha2Update(ctx[0], ms.Memory^, ms.Size);
    Sha2Final(ctx[0], digest);
    Result := digest.ToString();
  finally
    ms.Free;
  end;
end;

function applicaton_fullpath(): string;
begin
  Result := Win32.GetModuleFileName(0);
end;

function resource_to_file(Name: string; fileName: string): boolean;
var
  S: TResourceStream;
  F: TFileStream;
begin
  Result := False;
  try
    S := Win32.CreateResourceStream(Name);
    F := TFileStream.Create(fileName, fmCreate);
    try
      F.CopyFrom(S, S.Size);
      Result := True;
    finally
      F.Free;
    end;
  finally
    S.Free;
  end;
end;

function load_dll_from_resource_temp(Name: string): THandle;
var
  guid: TGuid;
  sTempDir: WideString;
  add2rcdllPath: WideString;
  add2rcdll: pwidechar;
  S: TResourceStream;
  F: TFileStream;
begin
  Result := 0;
  CreateGuid(guid);
  sTempDir := UTF8ToUTF16(temp_dir());
  add2rcdllPath := sTempDir + '\tmp.' +
    Utf8ToUtf16(ExtractFileName(applicaton_fullpath())) + '.' +
    Utf8ToUtf16(Name) + '.' + WideString(GuidToString(guid)) + '.dll';
  SafeWriteLn(add2rcdllPath);
  add2rcdll := @add2rcdllPath[1];
  if not resource_to_file(Name, UTF16ToUTF8(add2rcdllPath)) then
    Exit;
  //MoveFileExW(add2rcdll, nil, MOVEFILE_DELAY_UNTIL_REBOOT);
  Win32.RemoveFileOnReboot(add2rcdllPath);
  Result := Win32.LoadLibrary(add2rcdllPath);
end;

function load_dll_from_resource(Name: string): THandle;
var
  guid: string;
  sTempDir: WideString;
  add2rcdllPath: WideString;
  add2rcdll: pwidechar;
  S: TResourceStream;
  F: TFileStream;
begin
  Result := 0;
  guid := resource_sha256(Name);
  sTempDir := UTF8ToUTF16(temp_dir());
  add2rcdllPath := sTempDir + '\sha256.' +
    Utf8ToUtf16(ExtractFileName(applicaton_fullpath())) + '.' +
    Utf8ToUtf16(Name) + '.' + WideString(guid) + '.dll';
  SafeWriteLn(add2rcdllPath);
  add2rcdll := @add2rcdllPath[1];
  if FileExists(add2rcdllPath) then
  begin
    SafeWriteLn('reusing dll file');
  end
  else
  begin
    if not resource_to_file(Name, UTF16ToUTF8(add2rcdllPath)) then
      Exit;
  end;
  Result := Win32.LoadLibrary(add2rcdllPath);
end;

{ https://stackoverflow.com/questions/4605908/delphi-function-comparing-content-of-two-tstream }
function StreamsAreIdentical(Stream1, Stream2: TStream): boolean;
const
  Block_Size = 4096;
var
  Stream1_Pos: int64;
  Stream2_Pos: int64;
  Buffer_1: array[0..Block_Size - 1] of byte;
  Buffer_2: array[0..Block_Size - 1] of byte;
  Buffer_Length: integer;
begin
  Result := False;
  Stream1_Pos := Stream1.Position;
  Stream2_Pos := Stream2.Position;
  try
    if Stream1.Size <> Stream2.Size then exit;
    // These two added lines are critical for proper operation
    Stream1.Position := 0;
    Stream2.Position := 0;
    while Stream1.Position < Stream1.Size do
    begin
      Buffer_Length := Stream1.Read(Buffer_1, Block_Size);
      Stream2.Read(Buffer_2, Block_Size);
      if not CompareMem(@Buffer_1, @Buffer_2, Buffer_Length) then exit;
    end;
    Result := True;
  finally
    Stream1.Position := Stream1_Pos;
    Stream2.Position := Stream2_Pos;
  end;
end;

function FormatByteSize(ABytes: int64): string;
var
  dSize: double;
const
  KB = 1024;
  MB = KB * 1024;
  GB = MB * 1024;
  TB = GB * 1024;
  PB = TB * 1024;
begin
  Result := '';
  dSize := 0.0;
  if ABytes < 1024 then
  begin
    Result := IntToStr(ABytes) + ' B';
    exit;
  end;
  if ABytes < (MB) then
  begin
    dSize := ABytes / KB;
    Result := FormatFloat('0.##', dSize) + ' KB';
    exit;
  end;
  if ABytes < (GB) then
  begin
    dSize := ABytes / MB;
    Result := FormatFloat('0.##', dSize) + ' MB';
    exit;
  end;
  if ABytes < (TB) then
  begin
    dSize := ABytes / GB;
    Result := FormatFloat('0.##', dSize) + 'GB';
    exit;
  end;
  //if ABytes < (PB) then
  begin
    dSize := ABytes / TB;
    Result := FormatFloat('0.##', dSize) + ' TB';
  end;
end;

procedure msleep(msec: integer);
begin
  Sleep(msec);
end;

function SecondsToTimeString(n: integer): string;
var
  TotalSeconds: Integer;
  Hours, Minutes, Seconds: Integer;
  TimeString: String;
begin
  TotalSeconds := n;
  Hours := TotalSeconds div 3600;
  Minutes := (TotalSeconds mod 3600) div 60;
  Seconds := TotalSeconds mod 60;
  //TimeString := Format('%2.2d時間%2.2d分%2.2d秒', [Hours, Minutes, Seconds]);
  if Hours > 0 then TimeString := Format('%2.2d時間%2.2d分%2.2d秒', [Hours, Minutes, Seconds])
  else if Minutes > 0 then TimeString := Format('%2.2d分%2.2d秒', [Minutes, Seconds])
  else TimeString := Format('%2.2d秒', [Seconds]);
  Result := TimeString;
end;

function MillisecondsToTimeString(n: integer): string;
var
  TotalMilliseconds: Integer;
  Hours, Minutes, Seconds, Milli: Integer;
  TimeString: String;
begin
  TotalMilliseconds := n;
  Hours := TotalMilliseconds div 3600000;
  Minutes := (TotalMilliseconds mod 3600000) div 60000;
  Seconds := (TotalMilliseconds mod (3600000 div 60)) div 1000;
  Milli := TotalMilliseconds mod 1000;
  //TimeString := Format('%2.2d時間%2.2d分%2.2d.%3.3d秒', [Hours, Minutes, Seconds, Milli]);
  if Hours > 0 then TimeString := Format('%2.2d時間%2.2d分%2.2d.%3.3d秒', [Hours, Minutes, Seconds, Milli])
  else if Minutes > 0 then TimeString := Format('%2.2d分%2.2d.%3.3d秒', [Minutes, Seconds, Milli])
  else TimeString := Format('%2.2d.%3.3d秒', [Seconds, Milli]);
  Result := TimeString;
end;

end.
