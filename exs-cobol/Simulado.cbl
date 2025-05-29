       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULO-DELTA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-VARIAVEIS-ENTRADA.
           05 WS-COEFICIENTE-A PIC S9(04)V9(1).
           05 WS-COEFICIENTE-B PIC S9(04)V9(1).
           05 WS-COEFICIENTE-C PIC S9(04)V9(1).
           05 WS-OPCAO          PIC X(01).

       01 WS-CALCULOS.
           05 WS-B-QUADRADO PIC S9(09)V9(2).
           05 WS-4AC        PIC S9(09)V9(2).
           05 WS-DELTA      PIC S9(09)V9(2).

       01 WS-TABELA-RESULTADOS.
           05 WS-TABELA.
               10 WS-ITEM OCCURS 5 TIMES.
                   15 WS-VALOR      PIC S9(09)V9(2).
                   15 WS-VALOR-DISP PIC -Z(08)9,99.
           05 WS-DESCRICOES.
               10 PIC X(15) VALUE "1) DELTA:".
               10 PIC X(15) VALUE "2) VALOR DE A:".
               10 PIC X(15) VALUE "3) VALOR DE B:".
               10 PIC X(15) VALUE "4) VALOR DE B2:".
               10 PIC X(15) VALUE "5) VALOR DE C:".
           05 WS-TABELA-DESC REDEFINES WS-DESCRICOES.
               10 WS-DESC-ITEM PIC X(15) OCCURS 5 TIMES.
           05 WS-CONTADOR PIC 9(01).

       PROCEDURE DIVISION.

       PROGRAMA-PRINCIPAL.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESSAMENTO
               UNTIL WS-OPCAO = 'N' OR WS-OPCAO = 'n'.
           PERFORM 3000-FINALIZACAO.
           STOP RUN.

       1000-INICIO.
           DISPLAY "==================================================".
           DISPLAY " CALCULO DE DELTA (b*b - 4*a*c)".
           DISPLAY "==================================================".
           MOVE 'S' TO WS-OPCAO.

       2000-PROCESSAMENTO.
           PERFORM 2100-COLETAR-DADOS.
           PERFORM 2200-CALCULAR-DELTA.
           PERFORM 2300-PREENCHER-TABELA.
           PERFORM 2400-EXIBIR-RESULTADOS.
           PERFORM 2500-REINICIAR-PROGRAMA.

       2100-COLETAR-DADOS.
           DISPLAY "INFORME O COEFICIENTE 'a' (ex: 1234,5): ".
           ACCEPT WS-COEFICIENTE-A.
           DISPLAY "INFORME O COEFICIENTE 'b' (ex: 1234,5): ".
           ACCEPT WS-COEFICIENTE-B.
           DISPLAY "INFORME O COEFICIENTE 'c' (ex: 1234,5): ".
           ACCEPT WS-COEFICIENTE-C.

       2200-CALCULAR-DELTA.
           COMPUTE WS-B-QUADRADO = WS-COEFICIENTE-B * WS-COEFICIENTE-B.
           COMPUTE WS-4AC = 4 * WS-COEFICIENTE-A * WS-COEFICIENTE-C.
           COMPUTE WS-DELTA = WS-B-QUADRADO - WS-4AC.

       2300-PREENCHER-TABELA.
           MOVE WS-DELTA          TO WS-VALOR(1).
           MOVE WS-COEFICIENTE-A TO WS-VALOR(2).
           MOVE WS-COEFICIENTE-B TO WS-VALOR(3).
           MOVE WS-B-QUADRADO    TO WS-VALOR(4).
           MOVE WS-COEFICIENTE-C TO WS-VALOR(5).

       2400-EXIBIR-RESULTADOS.
           DISPLAY " ".
           DISPLAY "----------------- RESULTADOS ----------------".
           PERFORM VARYING WS-CONTADOR FROM 1 BY 1
               UNTIL WS-CONTADOR > 5
               MOVE WS-VALOR(WS-CONTADOR) TO WS-VALOR-DISP(WS-CONTADOR)
            DISPLAY WS-DESC-ITEM(WS-CONTADOR) WS-VALOR-DISP(WS-CONTADOR)
           END-PERFORM.

       2500-REINICIAR-PROGRAMA.
           DISPLAY "==================================================".
           DISPLAY "DESEJA EXECUTAR NOVAMENTE? (S/N)".
           ACCEPT WS-OPCAO.

       3000-FINALIZACAO.
           DISPLAY " ".
           DISPLAY "BOA AVALIACAO!!!!!".
           DISPLAY "==================================================".
