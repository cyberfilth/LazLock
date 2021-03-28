program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, splashabout
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Application Title';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

