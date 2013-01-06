program SimpleRestRequestSample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  RestRequest in 'RestRequest.pas';

var RestReq: TRestRequest;

begin
  try
    try
      RestReq := TRestRequest.Create();
    finally
      RestReq.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
