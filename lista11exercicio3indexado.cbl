      $set sourceformat"free"
      *>Divisão de identificação do programa
       identification division.
       program-id. "lista11exercicio3indexado".
       author. "Thays Popper".
       installation. "PC".
       date-written. 28/07/2020.
       date-compiled. 28/07/2020.



      *>Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.
           select arqAlunos assign to "arqAlunosIndexed.dat"
           organization is indexed
           access mode is dynamic
           lock mode is automatic
           record key is fd-aluno
           file status is ws-fs-arqAlunos.


       i-o-control.

      *>Declaração de variáveis
       data division.

      *>----Variaveis de arquivos
       file section.
       fd arqAlunos.
       01  fd-alunos.
           05  fd-aluno                            pic x(25).
           05  fd-endereco                         pic x(35).
           05  fd-mae                              pic x(25).
           05  fd-pai                              pic x(25).
           05  fd-tel                              pic x(15).
           05  fd-cod                              pic x(03).
           05 fd-notas.
               10 fd-nota1                         pic 9(02)v99.
               10 fd-nota2                         pic 9(02)v99.
               10 fd-nota3                         pic 9(02)v99.
               10 fd-nota4                         pic 9(02)v99.
               10 fd-media                         pic 9(02)v99.


      *>----Variaveis de trabalho
       working-storage section.

       77  ws-fs-arqAlunos                         pic  9(02).

       01  ws-alunos.
           05  ws-aluno                            pic x(25).
           05  ws-endereco                         pic x(35).
           05  ws-mae                              pic x(25).
           05  ws-pai                              pic x(25).
           05  ws-tel                              pic x(15).
           05  ws-cod                              pic x(03).

       01 ws-notas.
           05  ws-nota1                            pic 9(02)v99.
           05  ws-nota2                            pic 9(02)v99.
           05  ws-nota3                            pic 9(02)v99.
           05  ws-nota4                            pic 9(02)v99.
           05  ws-media                            pic 9(02)v99.

       01 ws-msn-erro.
          05 ws-msn-erro-ofsset                    pic 9(04).
          05 filler                                pic x(01) value "-".
          05 ws-msn-erro-cod                       pic 9(02).
          05 filler                                pic x(01) value space.
          05 ws-msn-erro-text                      pic x(42).





       77 ws-msn                                   pic  x(50).

       77 ws-sair                                  pic  x(01).
          88  fechar-programa                      value "N" "n".
          88  voltar-tela                          value "V" "v".
       77  ws-menu                                 pic  x(02).



      *>----Variaveis para comunicação entre programas
       linkage section.


      *>----Declaração de tela
       screen section.




      *>Declaração do corpo do programa
       procedure division.


           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  procedimentos de inicialização
      *>------------------------------------------------------------------------
       inicializa section.
      *>    inicializa menu

      *>================================================
      *> Open I-O Abre o Arquivo Para Leitura e Escrita
      *>================================================

           open i-o arqAlunos
           if ws-fs-arqAlunos  <> 00
           and ws-fs-arqAlunos <> 05 then
               move 1                                  to ws-msn-erro-ofsset
               move ws-fs-arqAlunos                    to ws-msn-erro-cod
               move "Erro ao abrir arq. arqTemp "      to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           .
       inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento Principal
      *>------------------------------------------------------------------------
       processamento section.

           perform until fechar-programa
               move space to ws-sair

               display "'Ca'dastrar Aluno"
               display "'Cn' Cadastrar Notas "
               display " 'Ci' Consulta Indexado"
               display "'Cs' Consulta sequencial"
               display "'De'letar"
               display "'Al'terar"

               accept ws-menu

               evaluate ws-menu
                   when = "Ca"
                       perform cadastrar-aluno

                    when = "Cn"
                       perform cadastrar-notas

                   when = "Ci"
                       perform cadastro-indexado

                   when = "Cs"
                       perform consultar-aluno-sequencial-next

                   when = "De"
                       perform deletar-aluno

                    when = "Al"
                       perform alterar-aluno

                   when other
                       display "Opcao Invalida"
               end-evaluate

            end-perform





           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  cadastro de aluno
      *>------------------------------------------------------------------------
       cadastrar-aluno section.

           perform until voltar-tela

               display " ======= Cadastro Alunos===== "
               display " Qual o nome do Aluno:"
               accept ws-aluno
               display " Qual o Endereco do Aluno:"
               accept ws-endereco
               display " Qual o nome da Mae?"
               accept ws-mae
               display " Qual o nome do Pai?"
               accept ws-pai
               display " Qual o Telefone:"
               accept ws-tel




      *>===============================================
      *> -------------  Salvar dados no arquivo ------
      *>===============================================

               write fd-alunos       from  ws-alunos
               if ws-fs-arqAlunos <> 0 then
                   move 2                                     to ws-msn-erro-ofsset
                   move ws-fs-arqAlunos                       to ws-msn-erro-cod
                   move "Erro ao escrever arq. arqTemp "      to ws-msn-erro-text
                   perform finaliza-anormal
               end-if

               display "Deseja consultar mais um Aluno? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform


           .
       cadastrar-aluno-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  cadastro de notas
      *>------------------------------------------------------------------------
       cadastrar-notas section.

           perform until voltar-tela

               display " ==== Cadastro Alunos ===="
               display " Informe o Cod do aluno:"
               accept ws-cod
               display " Informe a Nota 1:"
               accept ws-nota1
               display " Informe a Nota 2:"
               accept ws-nota2
               display " Informe a Nota 3:"
               accept ws-nota3
               display " Informe a Nota 4:"
               accept ws-nota4

              compute ws-media = (ws-nota1 + ws-nota2 + ws-nota3 + ws-nota4)/4


      *>================ Salvar Notas No Arquivo ====

               move ws-cod to fd-cod
               read arqAlunos


               if  ws-fs-arqAlunos <> 0 then
                   if ws-fs-arqAlunos = 23 then
                       display "Dado Informado Invalido!"
                   else
                       move 3                                     to ws-msn-erro-ofsset
                       move ws-fs-arqAlunos                       to ws-msn-erro-cod
                       move "Erro ao cadastrar arq. arqAlunos "   to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
                else
                   move ws-notas to fd-notas
                   rewrite fd-alunos
                       if  ws-fs-arqAlunos <> 0 then
                           move 4                                     to ws-msn-erro-ofsset
                           move ws-fs-arqAlunos                       to ws-msn-erro-cod
                           move "Erro ao gravar arq. arqAlunos "      to ws-msn-erro-text
                           perform finaliza-anormal
                   end-if

               end-if



               display "Deseja consultar mais um Aluno? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform


           .
       cadastrar-notas-exit.
           exit.


       cadastro-indexado section.
           perform until voltar-tela

           display " ==== Consulta Cadastro ==== "
           display " Informe o Cod Aluno:"
           accept ws-cod

      *>======================
      *> ler DADOS NO arquivo
      *>======================
           move ws-alunos to fd-alunos
           read arqAlunos

           if ws-fs-arqAlunos <> 0
           and ws-fs-arqAlunos <> 10 then
               if ws-fs-arqAlunos = 23 then
                   display " Cod Invalido!"
               else
                   move 5                                     to ws-msn-erro-ofsset
                   move ws-fs-arqAlunos                       to ws-msn-erro-cod
                   move "Erro ao gravar arq. arqAlunos "      to ws-msn-erro-text
                   perform finaliza-anormal
              end-if
           end-if

           move fd-alunos to ws-alunos

      *>================================================================
             display " Cod Aluno:" ws-cod
             display " Informe o nome do Aluno: " ws-aluno
             display " Informe o Endereco do Aluno: " ws-endereco
             display " Informe o nome do Pai:" ws-pai
             display " Informe o nome da Mae:" ws-mae
             display " Informe o Telefone: " ws-tel
             display " Nota 1" ws-nota1
             display " Nota 2" ws-nota2
             display " Nota 3" ws-nota3
             display " Nota 4" ws-nota4
             display " Media"  ws-media


             display "Deseja consultar mais um Aluno? 'S' ou 'V'oltar"
             accept ws-sair

           end-perform

           .

       cadastro-indexado-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Rotina de consulta de temperatura  - lê o arquivo de forma sequencial
      *>------------------------------------------------------------------------
       consultar-aluno-sequencial-next section.


           perform until voltar-tela

               read arqAlunos next
               if  ws-fs-arqAlunos <> 0  then
                  if ws-fs-arqAlunos = 10 then
                      perform consultar-aluno-sequencial-prev
                  else
                      move 6                                     to ws-msn-erro-ofsset
                      move ws-fs-arqAlunos                       to ws-msn-erro-cod
                      move "Erro ao ler arq. arqTemp "           to ws-msn-erro-text
                      perform finaliza-anormal
                  end-if
               end-if

               move  fd-alunos       to  ws-alunos

      *> ----------------------------------------------

               display "Cod: "  ws-cod
               display "Aluno: "  ws-aluno
               display " Mae: " ws-mae
               display " Pai: "  ws-pai
               display " Endereco:" ws-endereco
               display " Telefone:" ws-tel


               display "Deseja consultar mais um Aluno? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform


           .
       consultar-aluno-seq-next.
           exit.


      *>------------------------------------------------------------------------
      *>  Rotina de consulta de temperatura  - lê o arquivo de forma sequencial
      *>------------------------------------------------------------------------
       consultar-aluno-sequencial-prev section.


           perform until voltar-tela

               read arqAlunos previous
               if  ws-fs-arqAlunos <> 0  then
                  if ws-fs-arqAlunos = 10 then
                      perform consultar-aluno-sequencial-next
                  else
                      move 7                                     to ws-msn-erro-ofsset
                      move ws-fs-arqAlunos                       to ws-msn-erro-cod
                      move "Erro ao ler arq. arqTemp "           to ws-msn-erro-text
                      perform finaliza-anormal
                  end-if
               end-if

               move  fd-alunos       to  ws-alunos

      *> -------------
               display "Cod: "  ws-cod
               display "Aluno: "  ws-aluno
               display " Mae: " ws-mae
               display " Pai: "  ws-pai
               display " Endereco:" ws-endereco
               display " Telefone:" ws-tel


               display "Deseja consultar mais um Aluno? 'S' ou 'V'oltar"
               accept ws-sair


           end-perform


           .
       consultar-aluno-seq-prev-exit.
           exit.



      *>------------------------------------------------------------------------
      *>  Rotina de apagamento / Delete
      *>------------------------------------------------------------------------
       deletar-aluno section.

           perform until voltar-tela


      *> -------------  Apagar dados do registro do arquivo
               display "Informe o Aluno a ser excluido: "
               accept ws-aluno

               move ws-aluno to fd-aluno
               delete arqAlunos
               if  ws-fs-arqAlunos = 0 then
                   display " Aluno " ws-aluno " apagado com sucesso!"
               else
                   if ws-fs-arqAlunos = 23 then
                       display "Aluno informado invalido!"
                   else
                       move 8                                     to ws-msn-erro-ofsset
                       move ws-fs-arqAlunos                       to ws-msn-erro-cod
                       move "Erro ao apagar arq. arqAluno "       to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               end-if

               display "Deseja consultar mais um Aluno? 'S' ou 'V'oltar"
               accept ws-sair


           end-perform



           .
       deletar-aluno-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Rotina de alteração de Aluno
      *>------------------------------------------------------------------------
       alterar-aluno section.

           perform until voltar-tela


      *> -------------  Alterar dados do registro do arquivo
               display "Informe a novo Aluno:"
               accept ws-aluno

               move ws-aluno to fd-aluno
               rewrite fd-alunos
               if  ws-fs-arqAlunos = 0 then
                   display "Aluno " ws-aluno " alterado com sucesso!"
               else
                   move 9                                     to ws-msn-erro-ofsset
                   move ws-fs-arqAlunos                       to ws-msn-erro-cod
                   move "Erro ao alterar arq. arqAluno "      to ws-msn-erro-text
                   perform finaliza-anormal
               end-if

           display "Deseja consultar mais um Aluno? 'S' ou 'V'oltar"
           accept ws-sair


           end-perform

           .
       alterar-aluno-exit.
           exit.



      *>------------------------------------------------------------------------
      *>  Finalização  Anormal
      *>------------------------------------------------------------------------
       finaliza-anormal section.
           display erase
           display ws-msn-erro.
           accept  ws-msn-erro.
           stop run
           .
       finaliza-anormal-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Finalização
      *>------------------------------------------------------------------------
       finaliza section.

           close arqAlunos
           if ws-fs-arqAlunos <> 0 then
               move 10                                  to ws-msn-erro-ofsset
               move ws-fs-arqAlunos                     to ws-msn-erro-cod
               move "Erro ao fechar arq. arqAlunos "    to ws-msn-erro-text
               perform finaliza-anormal
           end-if



           stop run
           .
       finaliza-exit.
           exit.













