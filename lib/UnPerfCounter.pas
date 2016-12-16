unit UnPerfCounter;

interface

uses
    windows, SysUtils;

type
    {: Um contador de alta performance, utilizado para realizar medições de
    tempo/velocidade. }
    TPerformanceCounter = class(TObject)
    private
        startTick, endTick, freq: int64;
        countExecs: integer;
        accum: int64;
        FStarted: boolean;
    public
        constructor create;

        {: Inicia o cronômetro interno. }
        procedure start;

        {: Finaliza o cronômetro interno. }
        procedure stop;

        {: Retorna (em milisegundos) o tempo da última "corrida" do cronômetro.
        É preciso que os métodos start() e stop() tenham sido chamados
        anteriormente. }
        function lastMs: double;

        {: Retorna (em milisegundos) o tempo entre o último start() e agora. }
        function currentMs: double;

        {: Retorna a quantidade de execuções por segundo (baseado no acumulador). }
        function execsPerSec: double;

        {: Reinicializa o contador de execuções. }
        procedure zeroExecs;

        {: Zera a quantidade de execuções e o acumulador. }
        procedure zeroExecsAccum;

        {: Incrementa o contador de execuções em 1. }
        procedure incExecs;

        {: Indica se já iniciou o timer. }
        function started: boolean;

        {: Acumula o lastMs atual em um acumulador interno. }
        procedure accumulate;

        {: Pára a execução e acumula o que foi gasto. }
        procedure stopAndAccumulate;

        {: Retorna o tempo acumulado em millisegundos. }
        function currentAccumMS: double;

        {: Rertorna a média do acumulado pelo número de execuções. }
        function avgAccumMs: double;

        {: Zera o acumulador. }
        procedure zeroAccum;

        {: Retorna a quantidade de eventos acontecida em um determinado período. }
        function execCount: integer;

        {: Executa um writeln com o currentMs() }
        procedure currentToConsole(const msg: string);

        {: Executa um writeln com info do currentAccum(). }
        procedure accumToConsole(const msg: string);
    end;

var
    masterPerf: TPerformanceCounter;

implementation

{ TPerformanceCounter }

procedure TPerformanceCounter.accumToConsole(const msg: string);
begin
  writeln(Format('%8.2f - %d execs - %s', [self.currentAccumMS, self.countExecs, msg]));
end;

procedure TPerformanceCounter.accumulate;
begin
    accum:= accum + (endTick - startTick);
end;

function TPerformanceCounter.avgAccumMs: double;
begin
    if countExecs > 0 then
        result:= currentAccumMS / countExecs
    else
        result:= currentAccumMS;
end;

constructor TPerformanceCounter.create;
begin
    QueryPerformanceFrequency(freq);
end;

function TPerformanceCounter.currentAccumMS: double;
begin
    result:= accum / (freq/1000);
end;

function TPerformanceCounter.currentMs: double;
var
tick: int64;
begin
    QueryPerformanceCounter(tick);
    result:= (tick - startTick) / (freq/1000);
end;

procedure TPerformanceCounter.currentToConsole(const msg: string);
begin
    writeln(Format('%8.2f - %s', [self.currentMs, msg]));
end;

function TPerformanceCounter.execCount: integer;
begin
    Result:= countExecs;
end;

function TPerformanceCounter.execsPerSec: double;
var
curMs: double;
begin
    curMs:= currentAccumMS;
    result:= (curMs / 1000) / countExecs;
end;

procedure TPerformanceCounter.incExecs;
begin
    countExecs:= countExecs + 1;
end;

function TPerformanceCounter.lastMs: double;
begin
    result:= (endTick - startTick) / (freq/1000);
end;

procedure TPerformanceCounter.start;
begin
    QueryPerformanceCounter(startTick);
    FStarted:= true;
end;

function TPerformanceCounter.started: boolean;
begin
    result:= FStarted;
end;

procedure TPerformanceCounter.stop;
begin
    QueryPerformanceCounter(endTick);
    FStarted:= false;
end;

procedure TPerformanceCounter.stopAndAccumulate;
begin
    stop;
    accumulate;
    incExecs;
end;

procedure TPerformanceCounter.zeroAccum;
begin
    accum:= 0;
end;

procedure TPerformanceCounter.zeroExecs;
begin
    countExecs:= 0;
end;

procedure TPerformanceCounter.zeroExecsAccum;
begin
    zeroExecs;
    zeroAccum;
end;

initialization

    masterPerf:= TPerformanceCounter.create;
    masterPerf.start;
    {$IFDEF PerfConsoleAlertStart}
    masterPerf.currentToConsole('Iniciando Master Perf');
    {$ENDIF}

finalization

    masterPerf.Free;

end.
