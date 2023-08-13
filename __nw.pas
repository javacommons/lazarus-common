unit __nw {namespace my_sample};

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LazUtf8,
  __common,
  fphttpclient, opensslsockets,
  __fs
  {$ifdef MSWINDOWS}
  , __win32
  , __nw_res
  {$endif}
  ;

type
  TNetworkSystem = class
  private
    m_client: TFPHTTPClient;
    m_content_length: int64;

  public
    constructor Create();
    // Free で destructor が呼ばれるために override が必要
    destructor Destroy(); override;
    function DownloadFile(FileName: string; URL: string): boolean;
    function DownloadString(URL: string): string;
  protected
    procedure CB_OnHeaders(Sender: TObject);
    procedure CB_OnDataReceived(Sender: TObject;
      const ContentLength, CurrentPos: int64);
  end;

implementation

constructor TNetworkSystem.Create();
begin
  SafeWriteLn('TNetworkSystem.Create()');
  m_client := TFPHTTPClient.Create(nil);
  m_client.AllowRedirect := True;
end;

destructor TNetworkSystem.Destroy();
begin
  SafeWriteLn('TNetworkSystem.Destroy()');
  m_client.Free;
  inherited;
end;

function TNetworkSystem.DownloadString(URL: string): string;
begin
  m_client.AllowRedirect := True;
  Result := m_client.Get(URL);
end;

procedure TNetworkSystem.CB_OnHeaders(Sender: TObject);
var
  index: integer;
begin
  m_content_length := -1;
  for index := 0 to Pred(m_client.ResponseHeaders.Count) do
  begin
    if LowerCase(m_client.ResponseHeaders.Names[index]) = 'content-length' then
    begin
      m_content_length :=
        StrToInt64(m_client.ResponseHeaders.ValueFromIndex[index]);
    end;
  end;
end;

procedure TNetworkSystem.CB_OnDataReceived(Sender: TObject;
  const ContentLength, CurrentPos: int64);
var
  currentPercent: double = 0;
begin
  if m_content_length <> 0 then
    currentPercent := (CurrentPos * 100.0) / m_content_length;
  SafeWrite(#27'[1A'); // moves your cursor up one line, but in the same column
  SafeWrite(#13); // brings your cursor to the beginning of the line
  SafeWrite(#27'[2K'); // erases the entire line your cursor is currently on
  printf('  Progress: %f%% %s/%s'#10, [currentPercent, FormatByteSize(CurrentPos),
    FormatByteSize(m_content_length)]);
end;

function TNetworkSystem.DownloadFile(FileName: string; URL: string): boolean;
var
  guid: TGuid;
  temp: string;
begin
  printf('DownloadFile():'#10, []);
  printf('  FileName: %s:'#10, [FileName]);
  printf('  URL: %s:'#10, [URL]);
  SafeWriteLn();
  m_content_length := 0;
  CreateGuid(guid);
  temp := FileName + GuidToString(guid) + '.tmp';
  with m_client do
  begin
    OnHeaders := @CB_OnHeaders;
    OnDataReceived := @CB_OnDataReceived;
    AllowRedirect := True;
    Get(url, temp);
    Result := __fs.RenameFile(temp, FileName);
  end;
end;

var
  tempDir: WideString;
  dllPath: WideString;
  sha1: string;
  sha2: string;

initialization
begin
  {$ifdef MSWINDOWS}
  sha1 := resource_sha256('LIBEAY32_DLL');
  sha2 := resource_sha256('SSLEAY32_DLL');
  tempDir := Utf8ToUtf16(Format('%s\tmp.openssl%u.%s.%s', [temp_dir, os_bit, sha1, sha2]));
  echo(tempDir, 'tempDir');
  if SysUtils.DirectoryExists(tempDir) then
  begin
    echo('reusing openssl dlls');
    Exit;
  end;
  SysUtils.CreateDir(Utf16ToUtf8(tempDir));
  resource_to_file('LIBEAY32_DLL', Utf16ToUtf8(tempDir) + '\libeay32.dll');
  resource_to_file('SSLEAY32_DLL', Utf16ToUtf8(tempDir) + '\ssleay32.dll');
  dllPath := tempDir + '\libeay32.dll';
  Win32.LoadLibrary(dllPath);
  dllPath := tempDir + '\ssleay32.dll';
  Win32.LoadLibrary(dllPath);
  {$endif}
end;

end.
