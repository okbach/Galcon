program Galcon;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMain in 'UMain.pas' {frmMain},
  UGame in 'UGame.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.


