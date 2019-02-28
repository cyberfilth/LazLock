program lazlock;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  lazcontrols,
  { you can add units after this }
  unitmain,
  unitpassword,
  unittranslate;

{$R *.res}

begin
  Application.Title := 'LazLock';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TmainLazLock, mainLazLock);
  Application.CreateForm(TPasswordGenerator, PasswordGenerator);
  Application.Run;
end.
