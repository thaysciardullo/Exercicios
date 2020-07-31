      $set sourceformat"free"
      *>Divisão de identificação do programa
       identification division.
       program-id. "lista11exercicio3comtela".
       author. "Thays Popper ".
       installation. "PC".
       date-written. 30/07/2020.
       date-compiled. 30/07/2020.



      *>Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.
           select arqAlunos assign to "arqAlunos.dat"
           organization is indexed
           access mode is dynamic
           lock mode is automatic
           record key is fd-cod
           file status is ws-fs-arqAlunos.

       i-o-control.

      *>Declaração de variáveis
       data division.

      *>==========================
      *>----Variaveis de arquivos
      *>==========================

       file section.
       fd  arqAlunos.
       01  fd-alunos.
           05  fd-cod                              pic 9(03).
           05  fd-aluno                            pic x(25).
           05  fd-endereco                         pic x(35).
           05  fd-mae                              pic x(25).
           05  fd-pai                              pic x(25).
           05  fd-telefone                         pic x(15).
           05  fd-nota-g.
               10  fd-notas occurs 4.
                   15 fd-nota                      pic 9(02)v99.

      *>==========================
      *>----Variaveis de trabalho
      *>==========================

       working-storage section.

       77  ws-fs-arqAlunos                         pic x(02).

       01  ws-alunos.
           05  ws-cod                              pic 9(03).
           05  ws-aluno                            pic x(25).
           05  ws-endereco                         pic x(35).
           05  ws-mae                              pic x(25).
           05  ws-pai                              pic x(25).
           05  ws-telefone                         pic x(15).
           05  ws-nota-g.
               10  ws-notas occurs 4.
                   15  ws-nota
                   pic 9(02)v99.

       01 ws-aluno-rel-grup-total.
           05  ws-aluno-rel-grup occurs 15.
               10 ws-sel-rel                          pic x(01).
               10 ws-cod-rel                          pic 9(03).
               10 filler                              pic x(02)
                                                   value space.
               10 ws-aluno-rel                            pic x(12).
               10 filler                               pic x(02)
                                                       value space.
               10 ws-endereco-rel                         pic x(14).
               10 filler                               pic x(02)
                                                       value space.
               10 ws-mae-rel                              pic x(12).
               10 filler                               pic x(02)
                                                       value space.
               10 ws-pai-rel                              pic x(12).
               10 filler                               pic x(02)
                                                       value space.
               10 ws-tel-rel                              pic x(08).
               10 filler                               pic x(02)
                                                       value space.
               10 ws-media-rel                            pic 9(02)v99 value 0.


       77  ws-nota_aux                                pic 9(02)v99.
       77  ws-soma_nota                               pic 9(02)v99.
       77  ws-qtd_notas                               pic 9(1).


       77  ws-ind                                     pic 9(03).
       77  ws-ind1                                    pic 9(03).
       77  ws-ind2                                    pic 9(03).
       77  ws-ind-nota                                pic 9(03).



       77  ws-menu                                    pic x(02).
       77  ws-aux                                     pic x(01).

       01 ws-tela-menu.
          05  ws-cadastro-aluno                    pic  x(01).
          05  ws-cadastro-nota                     pic  x(01).
          05  ws-consulta-cadastro                 pic  x(01).
          05  ws-sair                              pic  x(01).

       77 ws-msn                                   pic  x(50).


       01 ws-msn-erro.
          05 ws-msn-erro-ofsset                    pic 9(04).
          05 filler                                pic x(01) value "-".
          05 ws-msn-erro-cod                       pic x(02).
          05 filler                                pic x(01) value space.
          05 ws-msn-erro-text                      pic x(42).

      *>----Variaveis para comunicação entre programas
       linkage section.

      *>========================
      *>----Declaração de tela
      *>========================

       screen section.
       01  tela-menu.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Tela Principal                                   ".
           05 line 03 col 01 value "      MENU                                                                       ".
           05 line 04 col 01 value "        [ ]Cadastro de Alunos                                                    ".
           05 line 05 col 01 value "        [ ]Cadastro de Notas                                                     ".
           05 line 06 col 01 value "        [ ]Consulta Cadastro                                                     ".

           05 sc-sair-menu            line 01  col 71 pic x(01)
           using ws-sair foreground-color 12.

           05 sc-cadastro-aluno       line 04  col 10 pic x(01)
           using ws-cadastro-aluno foreground-color 15.
           05 sc-cadastro-nota        line 05  col 10 pic x(01)
           using ws-cadastro-nota foreground-color 15.
           05 sc-consulta-cadastro    line 06  col 10 pic x(01)
           using ws-consulta-cadastro foreground-color 15.


       01  tela-cad-aluno.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Cadastro de Alunos                               ".
           05 line 03 col 01 value "      Cod      :                                                                                   ".
           05 line 04 col 01 value "      Aluno    :                                                                 ".
           05 line 05 col 01 value "      Endereco :                                                                 ".
           05 line 06 col 01 value "      Mae      :                                                                 ".
           05 line 07 col 01 value "      Pai      :                                                                 ".
           05 line 08 col 01 value "      Telefone :                                                                 ".
           05 line 22 col 01 value "              [__________________________________________________]               ".


           05 sc-sair-cad-alu            line 01  col 71 pic x(01)
           using ws-sair foreground-color 12.

           05 sc-cod-cad-alu           line 03  col 17 pic x(25)
           from ws-cod foreground-color 15.

           05 sc-aluno-cad-alu           line 04  col 17 pic x(25)
           using ws-aluno foreground-color 15.

           05 sc-endereco-cad-alu        line 05  col 17 pic x(35)
           using ws-endereco foreground-color 15.

           05 sc-mae-cad-alu             line 06  col 17 pic x(25)
           using ws-mae foreground-color 15.

           05 sc-pai-cad-alu             line 07  col 17 pic x(25)
           using ws-pai foreground-color 15.

           05 sc-tel-cad-alu             line 08  col 17 pic x(15)
           using ws-telefone foreground-color 15.


           05 sc-msn-cad-alu             line 22  col 16 pic x(50)
           using ws-msn foreground-color 15.




       01  tela-cad-notas.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Cadastro de Notas                                ".
           05 line 03 col 01 value "       Cod. Aluno:                                                               ".
           05 line 04 col 01 value "       Nota      :                                                               ".
           05 line 22 col 01 value "              [__________________________________________________]               ".




           05 sc-sair-cad-not         line 01  col 71 pic x(01)
           using ws-sair foreground-color 12.

           05 sc-cod-aluno            line 03  col 19 pic 9(03)
           using ws-cod foreground-color 15.

           05 sc-nota                 line 04  col 19 pic 9(02)v99
           using ws-nota_aux foreground-color 15.

           05 sc-msn-cad-not          line 22  col 16 pic x(50)
           using ws-msn foreground-color 15.


       01  tela-consulta-cad.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Consulta Cadastro                                ".
           05 line 03 col 01 value " Cod  Aluno         Endereco        Mae           Pai           Tel       Media  ".

           05 line 22 col 01 value "              [__________________________________________________]               ".

           05 sc-sair-con-cad         line 01  col 71 pic x(01)
           using ws-sair foreground-color 12.
           05 sc-cad-aluno1           line 04  col 01 pic x(80)
           using ws-aluno-rel-grup(1) foreground-color 12.
           05 sc-cad-aluno2           line 05  col 02 pic x(80)
           using ws-aluno-rel-grup(2) foreground-color 12.
           05 sc-cad-aluno3           line 06  col 02 pic x(80)
           using ws-aluno-rel-grup(3) foreground-color 12.
           05 sc-cad-aluno4           line 07  col 02 pic x(80)
           using ws-aluno-rel-grup(4) foreground-color 12.
           05 sc-cad-aluno5           line 08  col 02 pic x(80)
           using ws-aluno-rel-grup(5) foreground-color 12.
           05 sc-cad-aluno6           line 09  col 02 pic x(80)
           using ws-aluno-rel-grup(6) foreground-color 12.
           05 sc-cad-aluno7           line 10  col 02 pic x(80)
           using ws-aluno-rel-grup(7) foreground-color 12.
           05 sc-cad-aluno8           line 11  col 02 pic x(80)
           using ws-aluno-rel-grup(8) foreground-color 12.
           05 sc-cad-aluno9           line 12  col 02 pic x(80)
           using ws-aluno-rel-grup(9) foreground-color 12.
           05 sc-cad-aluno10          line 13  col 02 pic x(80)
           using ws-aluno-rel-grup(10) foreground-color 12.
           05 sc-cad-aluno11          line 14  col 02 pic x(80)
           using ws-aluno-rel-grup(11) foreground-color 12.
           05 sc-cad-aluno12          line 15  col 02 pic x(80)
           using ws-aluno-rel-grup(12) foreground-color 12.
           05 sc-cad-aluno13          line 16  col 02 pic x(80)
           using ws-aluno-rel-grup(13) foreground-color 12.
           05 sc-cad-aluno14          line 17  col 02 pic x(80)
           using ws-aluno-rel-grup(14) foreground-color 12.
           05 sc-cad-aluno15          line 18  col 02 pic x(80)
           using ws-aluno-rel-grup(15) foreground-color 12.



      *>Declaração do corpo do programa
       procedure division.


           perform inicializa.
           perform processamento.
           perform finaliza.

      *>=================================
      *>  Procedimentos de Inicialização
      *>=================================
       inicializa section.

      *>========================
      *>    Inicializa o Menu
      *>========================

           move  spaces      to     ws-menu

      *>================================================
      *> Open I-O Abre o Arquivo Para Leitura e Escrita
      *>================================================

           open i-o arqAlunos   *> open i-o abre o arquivo para leitura e escrita
           if ws-fs-arqAlunos  <> "00"
           and ws-fs-arqAlunos <> "05" then
               move 1                                to ws-msn-erro-ofsset
               move ws-fs-arqAlunos                  to ws-msn-erro-cod
               move "Erro ao abrir arq. arqAlunos "  to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           .
       inicializa-exit.
           exit.

      *>==========================
      *>  Processamento Principal
      *>==========================
       processamento section.

           perform until ws-sair = "X"
                      or ws-sair = "x"

      *>        inicialização das variaveis  da tela
                move   space  to  ws-cadastro-aluno
                move   space  to  ws-cadastro-nota
                move   space  to  ws-consulta-cadastro
                move   space  to  ws-sair

                display tela-menu
                accept tela-menu

                if ws-cadastro-aluno = "X"
                or ws-cadastro-aluno = "x" then
                       perform cadastrar-aluno
                end-if

                if ws-cadastro-nota = "X"
                or ws-cadastro-nota = "x" then
                       perform cadastrar-notas
                end-if

                if ws-consulta-cadastro = "X"
                or ws-consulta-cadastro = "x" then
                       perform consultar-cadastro
                end-if


           end-perform


           .
       processamento-exit.
           exit.

      *>========================
      *>  Cadastro de Aluno
      *>========================
       cadastrar-aluno section.

            perform until ws-sair = "V"
                       or ws-sair = "v"
               move spaces          to  ws-aluno
               move spaces          to  ws-endereco
               move spaces          to  ws-mae
               move spaces          to  ws-pai
               move spaces          to  ws-telefone

               perform buscar-prox-cod-alu

               display tela-cad-aluno
               accept tela-cad-aluno

               move spaces          to  ws-msn


               write fd-alunos from ws-alunos

               if ws-fs-arqAlunos  <> "00" then
                   move 1                                  to ws-msn-erro-ofsset
                   move ws-fs-arqAlunos                    to ws-msn-erro-cod
                   move "Erro ao gravar arq. arqAlunos "   to ws-msn-erro-text
                   perform finaliza-anormal
               end-if

            end-perform


           .
       cadastrar-aluno-exit.
           exit.


      *>============================
      *>  Alterar Cadastro de Aluno
      *>============================
       alterar-aluno section.

           move ws-cod          to fd-cod
           read arqAlunos       into ws-alunos
           if ws-fs-arqAlunos <> "00"
               move 1                                    to ws-msn-erro-ofsset
               move ws-fs-arqAlunos                      to ws-msn-erro-cod
               move "Erro ao Ler arq. arqAlunos "        to ws-msn-erro-text
               perform finaliza-anormal

           end-if


           display tela-cad-aluno
           accept tela-cad-aluno

           move spaces          to  ws-msn


           rewrite fd-alunos from ws-alunos
           if ws-fs-arqAlunos  <> "00" then
               move 1                                   to ws-msn-erro-ofsset
               move ws-fs-arqAlunos                     to ws-msn-erro-cod
               move "Erro ao Alterar arq. arqAlunos "   to ws-msn-erro-text
                   perform finaliza-anormal
           end-if



           .
       alterar-aluno-exit.
           exit.


      *>============================
      *>  Cadastro de Notas Alunos
      *>============================
       cadastrar-notas section.

           perform until ws-sair = "V"
                      or ws-sair = "v"

               perform consultar-cadastro *> Buscando os alunos dos quais quero cadastrar notas

               perform varying ws-ind from 1 by 1 until ws-ind > 15

                   if ws-sel-rel(ws-ind) = "S" then  *> Verificando quais alunos foram selecionados

                       move ws-cod-rel(ws-ind) to fd-cod *> Buscando os dados dos alunos selecionados
                                               ws-cod *> ----
                       read arqAlunos
                       if ws-fs-arqAlunos <> "00"
                           move 1                                    to ws-msn-erro-ofsset
                           move ws-fs-arqAlunos                      to ws-msn-erro-cod
                           move "Erro ao Ler arq. arqAlunos "      to ws-msn-erro-text
                           perform finaliza-anormal

                       end-if

                   move zero   to  ws-nota_aux

                   display tela-cad-notas *> Tela ---
                   accept tela-cad-notas

                   move space   to    ws-msn

                   perform buscar-prox-ind-nota  *> Com os dados do aluno já lidos do arquivo buscar a primeira posição livre dentro da tabela de notas
                   move ws-nota_aux to fd-nota(ws-ind1) *> Guarda nota no Arquivo

                   rewrite fd-alunos
                   if ws-fs-arqAlunos <> "00"  *> File Status Tratamento
                       move 1                                    to ws-msn-erro-ofsset
                       move ws-fs-arqAlunos                      to ws-msn-erro-cod
                       move "Erro de Gravar arq. arqAlunos "      to ws-msn-erro-text
                       perform finaliza-anormal

                   end-if

               end-if

           end-perform

           .
       cadastrar-notas-exit.
           exit.

      *>==========================
      *> Consultar Cadastro Aluno
      *>==========================
       consultar-cadastro section.

           move 1 to fd-cod
           start arqAlunos
           if ws-fs-arqAlunos <> "00"
               move 1                                    to ws-msn-erro-ofsset
               move ws-fs-arqAlunos                      to ws-msn-erro-cod
               move "Erro de Start arq. arqAlunos "      to ws-msn-erro-text
               perform finaliza-anormal

           end-if

           perform until ws-sair = "V"
                      or ws-sair = "v"

               perform varying ws-ind from 1 by 1 until ws-ind > 15
                                                    or ws-fs-arqAlunos = "10"


                    read arqAlunos next
                    if ws-fs-arqAlunos  = "00"
                    or ws-fs-arqAlunos = "10" then
                       if ws-fs-arqAlunos = "10" then
                           move "Fim de Aquivo " to ws-msn
                       else
                           move  fd-cod           to  ws-cod-rel(ws-ind)
                           move  fd-aluno         to  ws-aluno-rel(ws-ind)
                           move  fd-endereco      to  ws-endereco-rel(ws-ind)
                           move  fd-mae           to  ws-mae-rel(ws-ind)
                           move  fd-pai           to  ws-pai-rel(ws-ind)
                           move  fd-telefone      to  ws-tel-rel(ws-ind)
      *>                   CALCULO DA MEDIA
                       end-if
                    else
                        move 1                                  to ws-msn-erro-ofsset
                        move ws-fs-arqAlunos                    to ws-msn-erro-cod
                        move "Erro ao Ler arq. arqAlunos "      to ws-msn-erro-text
                        perform finaliza-anormal

                    end-if

      *>          metodo 1 para calcular a media
                   move 0    to ws-qtd_notas
                   move zero to ws-soma_nota

                   perform varying ws-ind2 from 1 by 1 until ws-ind2 > 4
                       if   ws-nota(ws-ind2)  is numeric then
                           compute ws-soma_nota = ws-soma_nota + ws-nota(4)
                           add 1 to ws-qtd_notas
                       end-if
                   end-perform


                   if ws-qtd_notas <> 0 then
                       compute ws-media-rel(ws-ind) = ws-soma_nota/ws-qtd_notas
                    else
                       move zero      to  ws-media-rel(ws-ind)

               end-perform

               display tela-consulta-cad
               accept tela-consulta-cad


               perform varying ws-ind from 1 by 1 until ws-ind > 15
                   if ws-sel-rel(ws-ind) = "X" then  *> Caso igual "X" desviar para deletar
                       move ws-cod-rel(ws-ind) to ws-cod
                       perform deletar-cadastro
                       initialize ws-aluno-rel-grup-total
                       perform consultar-cadastro
                   end-if

                   if ws-sel-rel(ws-ind) = "A" then  *> Caso igual "X" desviar para Alterar
                       move ws-cod-rel(ws-ind) to ws-cod
                       perform cadastrar-aluno
                       initialize ws-aluno-rel-grup-total


                   end-if
           end-perform
          end-perform

           .
       consultar-cadastro-exit.
           exit.

      *>==============================
      *>  Buscar Proximo Cod do Aluno
      *>==============================

       buscar-prox-cod-alu section.

            move 1 to fd-cod
            start arqAlunos
            if ws-fs-arqAlunos  = "00" then
               perform until ws-fs-arqAlunos = "10"
                   read arqAlunos next
                   if ws-fs-arqAlunos  <> "00"
                   and ws-fs-arqAlunos <> "10" then
                       move 1                                  to ws-msn-erro-ofsset
                       move ws-fs-arqAlunos                    to ws-msn-erro-cod
                       move "Erro ao Ler arq. arqAlunos "      to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
            end-perform

             move fd-cod to ws-cod
             add 1       to ws-cod
            else
               if ws-fs-arqAlunos = "23" then
                   move 1 to ws-cod
            else
               move 1                                    to ws-msn-erro-ofsset
               move ws-fs-arqAlunos                      to ws-msn-erro-cod
               move "Erro de Start arq. arqAlunos "      to ws-msn-erro-text
               perform finaliza-anormal

               end-if
           end-if


           .
       buscar-prox-cod-alu-exit.
           exit.

      *>================================
      *>  buscar proximo indice da nota
      *>================================
       buscar-prox-ind-nota section.
           perform varying ws-ind1 from 1 by 1 until ws-ind1 > 4
                                              or fd-nota(ws-ind1) is not numeric
               continue
           end-perform
           .
       buscar-prox-ind-nota-exit.
           exit.

      *>========================
      *>  deletar cadastro
      *>========================
       deletar-cadastro section.

           move ws-cod         to    fd-cod
           delete arqAlunos
           if ws-fs-arqAlunos  <> 00 then
               if ws-fs-arqAlunos = 23 then
                   display "Cod. Aluno Inexistente!"
               else
                   move 1                                  to ws-msn-erro-ofsset
                   move ws-fs-arqAlunos                    to ws-msn-erro-cod
                   move "Erro ao deletar arq. arqAlunos "  to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           end-if
           .
       deletar-cadastro-exit.
           exit.

      *>========================
      *>  Finalização  Anormal
      *>========================
       finaliza-anormal section.

           display erase
           display ws-msn-erro.
           stop run
           .
       finaliza-anormal-exit.
           exit.


      *>========================
      *>  Finalização
      *>========================
       finaliza section.

           close arqAlunos
           if ws-fs-arqAlunos  <> "00" then
               move 1                                  to ws-msn-erro-ofsset
               move ws-fs-arqAlunos                    to ws-msn-erro-cod
               move "Erro ao fechar arq. arqAlunos "   to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           stop run
           .
       finaliza-exit.
           exit.













