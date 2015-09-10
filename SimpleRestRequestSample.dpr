program SimpleRestRequestSample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Classes,
  RestRequest in 'RestRequest.pas';

var RestReq: TRestRequest;
    RestResp: THttpResponse;
begin
  try
    RestReq := nil;
    try
      RestReq := TRestRequest.Create().Domain('jsonplaceholder.typicode.com').Path('todos').Path('1');
      RestResp := RestReq.Get();
      if RestResp.ResponseCode = 200 then WriteLn('Your todo was added!') else WriteLn('Failed to add your todo.');
      WriteLn(RestResp.ResponseStr);
    finally
      RestReq.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
