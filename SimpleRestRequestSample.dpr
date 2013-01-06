program SimpleRestRequestSample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Classes,
  RestRequest in 'RestRequest.pas';

var RestReq: TRestRequest;
    RestResp: THttpResponse;
    putParams: TStringList;
begin
  try
    try
      putParams := TStringList.Create();
      putParams.Add('title=Buy milk');
      putParams.Add('due-date=01/01/2013 00:00:00');
      RestReq := TRestRequest.Create().Domain('localhost').Path('todo').WithCredentials('test', 'test');
      RestResp := RestReq.Put(putParams);
      if RestResp.ResponseCode = 200 then WriteLn('Your todo was added!');
    finally
      RestReq.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
