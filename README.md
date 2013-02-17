## Overview
SimpleRestClient is a very simple, fluent wrapper around the Indy IdHttp client to make it easy to write RESTful clients.

## Usage
This very simple console application creates a RESTful PUT request to add a fictional todo.

```delphi
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
      if RestResp.ResponseCode = 201 then WriteLn('Your todo was added!');
    finally
      RestReq.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
```

## Limitations and improvements
SimpleRestClient was thrown together pretty quickly to fill a need that I had and has limitations in areas that I didn't need or have time to do. 
* Test coverage - There is some test coverage but I didn't have time to mock out the Indy IdHttp client.
* Patch support - The indy client doesn't support the PATCH request at present.
* Accept headers - I'd like a better solution (read: more Delphi like) to Accept headers.

## Licensing
Copyright (c) 2013, Jamie Ingilby
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
Neither the name of SimpleRestClient for Delphi nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.