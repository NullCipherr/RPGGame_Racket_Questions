#lang racket

(require rackunit)
(require rackunit/text-ui)





;; ---------------------------------------------------------- ;;
;; Alunos e RA
;; ---------------------------------------------------------- ;;

; Andrei Roberto da Costa ................. 107975
; João Gilberto Pelisson Casagrande ....... 112684

;; ---------------------------------------------------------- ;;





; ------------------------------------------------------------ ;; 
; 1. Estruturas
; ------------------------------------------------------------ ;;

; Estrutura que representa um jogador no jogo.
; id: Identificador único do jogador.
; name: Nome do jogador.
; difficulty: Nível de dificuldade escolhido pelo jogador.
; level: Nível atual do jogador na jornada do jogo.
; experience: Quantidade total de experiência acumulada pelo jogador ao longo da jornada.
; stage: Estágio atual ou posição do jogador no jogo.
(struct player (id name difficulty level experience stage) #:transparent)





; Estrutura que representa um nível no jogo.
; number-level: Número do nível. Pode conter valores como 1, 2, 3, ..., n.
; name: Nome atribuído ao nível.
; concept: Descrição breve ou conceito do que o nível representa. Pode explicar os desafios que os jogadores enfrentarão ou os conhecimentos que adquirirão ao completar o nível.
; answer: Resposta correta do nível.
; reward-experience: Quantidade de experiência que um jogador ganha ao completar o nível. Pode ser um valor numérico acumulado para avançar nos níveis do jogador ou desbloquear recursos adicionais.
; answer-correct-message: Mensagem associada à resposta correta, fornecendo feedback positivo ao jogador quando ele resolve corretamente o enigma do nível.
(struct level (number-level name concept answer reward-experience answer-correct-message) #:transparent)





; ===================================================================================================================================================

; (make-player: Cria e retorna um jogador com as informações fornecidas.
;
; Parâmetros:
;   - id: Identificador único do jogador.
;   - name: Nome do jogador.
;   - role: Papel ou função do jogador no jogo (ex: "guerreiro", "mago").
;   - level: Nível atual do jogador.
;   - experience: Quantidade de experiência acumulada pelo jogador.
;   - advance: Avanço ou progresso adicional do jogador.
; 
(define (make-player id name role level experience stage)
  ; Cada elemento da lista é uma cons célula que associa uma chave simbólica ao valor correspondente.
  (player id name role level experience stage))



; Inicializa o contador de criação de personagens
(define character-counter 0)


; --------------------------------------------------------
; Seção : Funções Auxiliares
; --------------------------------------------------------

;; Teste ... -> Void
;; Executa um conjunto de testes.
(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))





;; String -> String
;; Função que tem como uma entrada uma string e retorna uma string sem os espaços em branco.
;; (define (remove-spaces str) ...)
;;
(define (remove-spaces str)
  (string->list str) ; Converte a string para uma lista de caracteres
  (list->string (filter (lambda (c) (not (char-whitespace? c))) (string->list str))))

;; Teste unitário -> OKAY
(define (remove-space-tests)
  (test-suite "remove space tests"
              (check-equal? (remove-spaces "Hello World") "HelloWorld")
              (check-equal? (remove-spaces "   Racket  ") "Racket")
              (check-equal? (remove-spaces "NoSpacesHere") "NoSpacesHere")
              (check-equal? (remove-spaces "") "")))





;; Void -> String
;; Aguarda que o usuário pressione Enter e retorna a linha inserida.
;; (define (wait-enter) ...)
;;
(define (wait-enter)
  (flush-output)
  (read-line))





;; Void -> Void
;; Exibe uma mensagem de saída e encerra o programa.
;; (define (exit-game) ...)
;;
(define (exit-game)
  (newline)
  (displayln " Saindo do Jogo...")
  (displayln " Muito obrigado por jogar Aventuras na Terra Racket!!")
  (newline)
  (exit))






;; String -> Void
;; A função write imprime uma string na tela, letra por letra, com um atraso de 0.03 segundos entre cada letra, simulando o efeito de digitação de texto em tempo real.
;; (define (write string) ...)
;;
(define (write string)
  ;; Define um tempo de atraso de 0.03 segundos entre cada caractere impresso
  (define time-write 0.0025)
  ;; Converte a string em uma lista de caracteres
  (define chars (string->list string))
  ;; Define uma função auxiliar 'loop' que recebe uma lista de caracteres
  (define (loop chars)
    ;; Verifica se a lista de caracteres não está vazia
    (unless (empty? chars)
      ;; Imprime o primeiro caractere da lista
      (display (first chars))
      ;; Esvazia o buffer de saída para garantir que o caractere seja exibido imediatamente
      (flush-output)
      ;; Pausa a execução do programa pelo tempo definido
      (sleep time-write)
      ;; Chama a função 'loop' recursivamente com o restante da lista de caracteres
      (loop (rest chars))))
  ;; Chama a função 'loop' com a lista de caracteres
  (loop chars))





;; String -> Boolean
;; Função para verificar se uma string contém letras maiúsculas, retorna verdadeiro se a string contiver pelo menos uma letra maiúscula, senão, falso.
;; (define (has-uppercase? str) ...)
;;
(define (has-uppercase? str)
  (regexp-match? #rx"[A-Z]" str))

;; Teste unitário
(define (has-uppercase?-tests)
  (test-suite "has uppercase tests"
  (check-equal? (has-uppercase? "") #f)
  (check-equal? (has-uppercase? "abc") #f)
  (check-equal? (has-uppercase? "ABC") #t)
  (check-equal? (has-uppercase? "aBc") #t)))





; Função para verificar se cada abertura de parênteses tem um correspondente fechamento na string.
; String -> Boolean
; Retorna verdadeiro se cada abertura de parênteses tiver um correspondente fechamento na string, senão, falso.
; (define (has-matching-parentheses? str) ...)
;
(define (has-matching-parentheses? str)
  (define (check-parentheses str count)
    (cond
      [(= count -1) #f]               ; Mais fechamentos do que aberturas
      [(= (string-length str) 0) (= count 0)] ; Contagem final deve ser zero
      [(char=? (string-ref str 0) #\() (check-parentheses (substring str 1) (+ count 1))] ; Se encontrar um abre parênteses, incrementa a contagem.
      [(char=? (string-ref str 0) #\)) (check-parentheses (substring str 1) (- count 1))] ; Se encontrar um fecha parênteses, decrementa a contagem.
      [else (check-parentheses (substring str 1) count)])) ; Caso contrário, ignora o caractere e continua a verificação.
  (check-parentheses str 0)) ; Começa a verificação com contagem zero.

; Teste unitário
(define (has-matching-parentheses?-tests)
  (test-suite "has matching parentheses? tests"
  (check-equal? (has-matching-parentheses? "") #t)
  (check-equal? (has-matching-parentheses? "()") #t)
  (check-equal? (has-matching-parentheses? "(())") #t)
  (check-equal? (has-matching-parentheses? "(()())") #t)
  (check-equal? (has-matching-parentheses? "(()))") #f)
  (check-equal? (has-matching-parentheses? "(()()") #f)))




;; Function Function -> Function
;;
(define (process-input function-a function-b value)
  (display " -> ")
  (let ((x (read-line)))
    (if (= (string->number x) value)
        (function-a)
        (function-b))))


; --------------------------------------------------------
; Seção : Jogador
; --------------------------------------------------------

;; Player -> Void
;; Função que desenha o HUD (Head-Up Display) do jogador, exibindo informações como nome, nível, experiência, estágio e dificuldade.
;; (define (draw-player-hud player) ...)
;; Parâmetros:
;   - player: Estrutura Player contendo as informações do jogador.
;;
(define (draw-player-hud player)
  (define blank-space "          ")
  (newline)
  (displayln "    ╔═════════════════════════════════════════════════════════════════════════════════════╗ ")
  (displayln "    ║            Name          Level          XP          Stage          Difficulty       ║ ")
  (display   "    ╚═════════════════════════════════════════════════════════════════════════════════════╝ ")
  (newline)
  (display "    ║")
  (display blank-space)
  (display (player-name player))

  (display "           ")
  (display (player-level player))

  (display "             ")
  (display (player-experience player))
  (display "             ")
  (display (player-stage player))
  (display "             ")
  (display (player-difficulty player))
  (display "       ")
  (newline)
  (displayln "    ═══════════════════════════════════════════════════════════════════════════════════════ ")
  (newline))





;; Void -> String
;; Solicita ao usuário que escolha um nome e retorna a entrada do usuário como uma string.
;; (define (set-name) ...)
;;
(define (set-name)
  (display-select-name)
  (display "\n Nome -> ")
  (flush-output)
  (read-line))





;; String -> String
;; Define a dificuldade do jogo com base na entrada do usuário.
;; (define (set-difficulty difficulty) ...)
;;
(define (set-difficulty difficulty)
  (if (equal? difficulty "Easy")
        "Easy"
        (if (equal? difficulty "Medium")
            "Medium"
            (if (equal? difficulty "Hard")
                "Hard"
                (error "Comando inválido!")))))

;; Teste unitário
(define (set-difficulty-tests)
  (test-suite "set difficulty tests"
  (check-equal? (set-difficulty "Easy") "Easy")
  (check-equal? (set-difficulty "Medium") "Medium")
  (check-equal? (set-difficulty "Hard") "Hard")))





;; Void -> void
;; Função para inicializar a criação do jogo, permitindo ao jogador criar um novo personagem.
;; A função apresenta a narrativa inicial e oferece a opção de criar um novo personagem se nenhum personagem foi criado anteriormente.
;; (define (initialize-create) ...)
;;

(define (initialize-create)
  (newline)
  ; Verifica se é a primeira vez que o jogador está criando um personagem.
  (if (= character-counter 0)
      (write " Parece que você não criou nenhum personagem ainda. A seguir, selecione a opção para criarmos o seu guerreiro!!")
      (write " Bem-vindo de volta, guerreiro.")
  )
  (newline)
  (newline)
  (write "  1. Criar personagem")
  (newline)
  (write "  2. Voltar ao menu")
  (newline)
  (newline)
  (process-input create-player menu-game 1))





;; Void -> Void
;; Cria um novo personagem com 'id', nome, dificuldade, nível, experiência e avanço inicial.
;; (define (create-player) ...)
;; 
(define (create-player)
  (define id
    (if (= character-counter 0) 1 ; Se o contador de personagens for zero, define o ID como 1.
        (error "Você já criou o seu personagem!!"))) ; Caso contrário, gera um erro.

  (display-select-name) ; Exibe a mensagem de seleção de nome.
  (display "\n Nome -> ")
  (let ((name (read-line))) ; Lê o nome do jogador.

    (display-select-difficulty name) ; Exibe a mensagem de seleção de dificuldade.
    (display "\n Difficulty -> ")
    (let* ((difficulty_temp (read-line)) ; Lê a dificuldade do jogador.
           (difficulty (verify-difficulty-input difficulty_temp)) ; Verifica se a dificuldade é válida.
           (stage 1) ; Independente da dificuldade, o player começará no stage=1, de sua respectiva dificuldade.
           (level 1)
           (experience 0)
           (character-1 (make-player id name difficulty level experience stage))) ; Cria um novo personagem com as informações fornecidas.
    
      (start-game character-1)))) ; Inicia o jogo com o novo personagem.





;; String Numero -> Boolean
;; Verifica se o jogador atingiu a quantidade de experiência necessária para subir de nível, com base na dificuldade.
;; (define (verify-level-up difficulty experience) ...)
;;
(define (verify-level-up difficulty experience)
  (cond
    [(string=? difficulty "Easy") (>= experience 200)]
    [(string=? difficulty "Medium") (>= experience 400)]
    [(string=? difficulty "Hard") (>= experience 600)]
    ))

;; Teste unitário
(define (verify-level-up-tests)
  (test-suite "verify level up tests"
              (check-equal? (verify-level-up "Easy" 199) #f)
              (check-equal? (verify-level-up "Easy" 200) #t)
              (check-equal? (verify-level-up "Medium" 399) #f)
              (check-equal? (verify-level-up "Medium" 400) #t)
              (check-equal? (verify-level-up "Hard" 599) #f)
              (check-equal? (verify-level-up "Hard" 600) #t)))




;; String -> String
;; Retorna a próxima dificuldade, considerando que a progressão é de Easy para Medium e de Medium para Hard.
;; (define (next-difficulty current-difficulty) ...)
;;
(define (next-difficulty current-difficulty)
  (cond
    [(equal? current-difficulty "Easy") "Medium"]
    [(equal? current-difficulty "Medium") "Hard"]
    [else "Hard"])) ; Não muda a dificuldade além de 'Hard'

;; Teste unitário
(define (next-difficulty-tests)
  (test-suite "next difficulty tests"
              (check-equal? (next-difficulty "Easy") "Medium")
              (check-equal? (next-difficulty "Medium") "Hard")
              (check-equal? (next-difficulty "Hard") "Hard")))



; --------------------------------------------------------
; Seção : Jogo
; --------------------------------------------------------


;; String -> Void
;; Exibe uma mensagem de seleção de nível de dificuldade para o usuário.
;; (define (display-select-difficulty name) ...)
;;
(define (display-select-difficulty name)
  (define border-up "                                 ╔═════════════════════╗ ")
  (define border-down "                                 ╚═════════════════════╝ ")
  (define empty-space "                                     ")

  (newline)
  (displayln " ╔════════════════════════════════════════════════════════════════════════════════════════╗ ")
  (displayln (format " ║     Saudações ~a,
 ║   a seleção do nível de dificuldade é um momento crucial, pois determinará             ║
 ║   a intensidade e a complexidade das situações que você enfrentará.                    ║
 ║                                                                                        ║
 ║   Cada opção oferece uma experiência única, adaptada ao seu estilo de jogo e apetite   ║
 ║   por desafios. Portanto, ao realizar essa escolha, leve em consideração suas habi-    ║
 ║   lidades, experiência anterior em jogos semelhantes e, acima de tudo, a disposição    ║
 ║   para enfrentar adversidades.                                                         ║
 ║                                                                                        ║
 ║   Lembre-se de que cada dificuldade oferecerá uma jornada única, com recompensas pro-  ║  
 ║   porcionais aos desafios superados.                                                   ║" name))
  (displayln " ╚════════════════════════════════════════════════════════════════════════════════════════╝ ")
  (newline)

  (displayln border-up)
  (displayln "                                 ║      1. Easy        ║")
  (displayln "                                 ║      2. Medium      ║")
  (displayln "                                 ║      3. Hard        ║")
  (display border-down)
  (newline))





;; String -> String
;; Define a dificuldade do jogo com base na entrada do usuário.
;; (define (verify-difficulty-input difficulty) ...)
;;
(define (verify-difficulty-input difficulty)
  (cond [(equal? difficulty "Easy") (set-difficulty difficulty)]
        [(equal? difficulty "Medium") (set-difficulty difficulty)]
        [(equal? difficulty "Hard") (set-difficulty difficulty)]
        [(= (string->number difficulty) 1) (verify-difficulty-input "Easy")]
        [(= (string->number difficulty) 2) (verify-difficulty-input "Medium")]
        [(= (string->number difficulty) 3) (verify-difficulty-input "Hard")]
        [else (error "Dificuldade inválida!")]))

;; Teste unitário
(define (verify-difficulty-input-tests)
  (test-suite "verify difficulty input tests"
  (check-equal? (verify-difficulty-input "Easy") "Easy")
  (check-equal? (verify-difficulty-input "Medium") "Medium")
  (check-equal? (verify-difficulty-input "Hard") "Hard")
  (check-equal? (verify-difficulty-input "1") "Easy")
  (check-equal? (verify-difficulty-input "2") "Medium")
  (check-equal? (verify-difficulty-input "3") "Hard")))





;; Player -> void
;; Função que inicializa o jogo, desenhando o HUD do jogador e iniciando o primeiro nível.
;; (define (initialize-game player) ...)
;; Parâmetros:
;   - player: Estrutura Player contendo as informações do jogador.
(define (initialize-game player)
  (draw-player-hud player)
  (select-level player (player-difficulty player) (player-stage player)))





;; Player String Int -> Void
;; Função que inicializa o nível atual do personagem com base na dificuldade e estágio fornecidos.
;; (define (select-level player difficulty stage) ...)
;; Parâmetros:
;   - player: Estrutura Player contendo as informações do jogador.
;   - difficulty: String representando a dificuldade do nível (Easy, Medium, Hard).
;   - stage: Inteiro representando o estágio do nível (1, 2, 3).
;
(define (select-level player difficulty stage)
  (cond
    ((equal? difficulty "Easy")
     (cond
      ((= stage 1) (start-level level-1-easy player))
      ((= stage 2) (start-level level-2-easy player))
      ((= stage 3) (start-level level-3-easy player))))

    ((equal? difficulty "Medium")
     (cond
      ((= stage 1) (start-level level-1-medium player))
      ((= stage 2) (start-level level-2-medium player))
      ((= stage 3) (start-level level-3-medium player))))

    ((equal? difficulty "Hard")
     (cond
      ((= stage 1) (start-level level-1-hard player))
      ((= stage 2) (start-level level-2-hard player))
      ((= stage 3) (start-level level-3-hard player))))))




;; Player -> Void
;; Exibe os créditos e redireciona para o menu de personagem criado.
;; (define (print-credits-created? player) ...)
;; Parâmetros:
; - player: O jogador cujo personagem foi criado.
;
(define (print-credits-created? player)
  (display-credits)
  (process-input (lambda () (menu-game-character-created? player)) print-credits-created? 1))



;; Player -> Void
;; Exibe os créditos e redireciona para o menu de personagem criado.
;; (define (print-help-created? player) ...)
;; Parâmetros:
; - player: O jogador cujo personagem foi criado.
;
(define (print-help-created? player)
  (display-credits)
  (process-input (lambda () (menu-game-character-created? player)) print-help-created? 1))





;; Player -> Void
;; Função que controla as seleções do menu, permitindo ao jogador escolher entre as ações disponíveis.
;; (define (menu-select) ...)
;
(define (menu-select-character-created? player)
  (display " -> ")
  (let ((choose (string->number (read-line))))
    (cond
      ((= choose 1) (start-game player))
      ((= choose 2) (print-help-created? player))
      ((= choose 3) (print-credits-created? player))
      ((= choose 4) (exit-game))
      (else (menu-game-character-created? player)))))







;; Player -> Void
;; Exibe o menu do jogo e redireciona para a seleção de personagem criado.
;; (define (menu-game-character-created? player) ...)
;;
(define (menu-game-character-created? player)
  (display-menu-game)
  (menu-select-character-created? player))



;; Player -> Void
;; Exibe um menu de opções para o jogador e executa a ação correspondente à escolha do jogador.
;; (define (start-game player) ...)
;;
(define (start-game player)
  (draw-player-hud player)
  (newline)
  (display (format " Bem Vindo ~a, \n
             1. Começar estágios
             2. Mostrar estágios desbloqueados
             3. Voltar ao menu \n\n" (player-name player)))

  (display " -> ")
  (let ((option (string->number (read-line))))
    (cond
      ((= option 1) (initialize-game player))
      ((= option 2) (display-unlocked-stages player))
      ((= option 3) (menu-game-character-created? player)) ; Supondo que menu-select seja a função principal do menu
      (else (start-game player)))))




;; String -> Void
;; Exibe as fases desbloqueadas com base na dificuldade fornecida.
;; (define (show-stages-unlocked difficulty) ...)
;; Parâmetros:
;; - difficulty: A dificuldade para a qual mostrar as fases desbloqueadas.
;
(define (show-stages-unlocked difficulty)
  (newline)
   (cond
    [(string=? difficulty "Easy")(display "  Easy[Unlocked] -> Medium[Locked] -> Hard[Locked]")]
    [(string=? difficulty "Medium")(display "  Easy[Unlocked] -> Medium[Unlocked] -> Hard[Locked]")]
    [(string=? difficulty "Hard")(display "  Easy[Unlocked] -> Medium[Unlocked] -> Hard[Unlocked]")]
    )
  (newline)
  (wait-enter))




;; Player -> Void
;; Exibe as fases desbloqueadas com base na dificuldade do jogador e volta para o menu do personagem.
;; (define (display-unlocked-stages player) ...)
;; Parâmetros:
;; - player: O jogador cujo personagem foi criado.
;;
(define (display-unlocked-stages player)
  (let ((difficulty (player-difficulty player)))
  (cond
    [(string=? difficulty "Easy")(show-stages-unlocked difficulty)]
    [(string=? difficulty "Medium")(show-stages-unlocked difficulty)]
    [(string=? difficulty "Hard")(show-stages-unlocked difficulty)]))
  (start-game player))





;; Player Level -> void
;; Atualiza o jogador com base no progresso do nível concluído, incluindo subir de nível, mudar de dificuldade, etc.
;; (define (save-progress player level) ...)
;;
(define (save-progress player level)
  (define new-name (player-name player)) ; Define o novo nome do jogador.
  (define new-id (player-id player)) ; Define o novo ID do jogador.
  (define new-experience (+ (player-experience player) (level-reward-experience level))) ; Define a nova quantidade de experiência do jogador.

  (cond 
    [(verify-level-up (player-difficulty player) new-experience) ; Se o jogador atingiu a quantidade de experiência necessária para subir de nível:
     (let* ((reset-experience 0) ; Define a quantidade de experiência do jogador como zero, por causa do reset de experiencia, ao subir de nível.
            (new-difficulty (next-difficulty (player-difficulty player))) ; Define a nova dificuldade do jogador.
            (new-level (+ (player-level player) 1)) ; Define o novo nível do jogador.
            (new-stage 1) ; Define a nova fase do jogador.
            (new-character (make-player new-id new-name new-difficulty new-level reset-experience new-stage))) ; Cria um novo personagem com as novas informações.
       (notify-level-up new-level) ; Notifica o jogador que ele subiu de nível.
       (draw-player-hud new-character) ; Desenha o HUD do jogador com as novas informações.
       (select-level new-character new-difficulty new-stage))] ; Seleciona a próxima fase do jogo.
    
    [else ; Caso contrário:
     (let* ((new-difficulty (player-difficulty player)) ; Mantém a mesma dificuldade do jogador.
            (new-level (player-level player)) ; Mantém o mesmo nível do jogador.
            (new-stage (+ (player-stage player) 1)) ; Define a nova fase do jogador.
            (new-character (make-player new-id new-name new-difficulty new-level new-experience new-stage))) ; Cria um novo personagem com as novas informações.
       (draw-player-hud new-character) ; Desenha o HUD do jogador com as novas informações.
       (select-level new-character new-difficulty new-stage))])) ; Seleciona a próxima fase do jogo.





;; Player Level -> void
;; Exibe mensagens e solicita ação do jogador para continuar para o próximo estágio ou repetir o nível, após a resposta estiver correta.
;; (define (next-stage-input player level) ...)
;;
(define (next-stage-input player level)
  (newline)
  (newline)
  (displayln "  1. Continuar")
  (displayln "  2. Voltar ao menu")
  (newline)
  (display " -> ")
  (let ((x (read-line)))
    (cond
      [(= (string->number x) 1) (save-progress player level)]
      [(= (string->number x) 2) (start-game player)]
      [else (next-stage-input player level)])))





;; Void -> Void
;; Exibe mensagens de resposta incorreta e solicita ação do jogador para continuar.
;; (define (incorrect-answer) ...)
;
(define (incorrect-answer)
  (newline)
  (display " A sua resposta está incorreta")
  (newline)
  (display " Pressione Enter para tentar novamente...")
  (newline)
  (wait-enter))





;; Level Player -> Void
;; Exibe informações sobre o nível atual, solicita a resposta do jogador e executa ações com base na resposta.
;; (define (start-level level player) ...)
;
(define (start-level level player) 
  (define border " ------------------------------------------------------------------------")
  
  (displayln border) ; Exibe uma linha de separação.
  (displayln (format " Nível ~a: ~a" (level-number-level level) (level-name level))) ; Exibe o número e o nome do nível.
  (newline)
  (write (level-concept level)) ; Exibe o conceito do nível.
  (newline)
  (newline)
  (displayln (format " Recompensa de Experiência: ~a" (level-reward-experience level))) ; Exibe a recompensa de experiência do nível.
  (displayln border) ; Exibe uma linha de separação.

  (display " Digite a sua resposta: ") ; Solicita a resposta do usuário.
  (flush-output)
  (let ((user-answer (read-line))) ; Lê a resposta do usuário.
    (if (string=? (remove-spaces user-answer) (remove-spaces (level-answer level))) ; Se a resposta estiver correta:
        (begin
          (newline)
          (write (level-answer-correct-message level)) ; Exibe a mensagem de resposta correta.
          (next-stage-input player level) ; Avança para a próxima fase.
          (select-level player (player-difficulty player) (+ (player-stage player) 1))) ; Seleciona o próximo nível.
        (begin ; Caso contrário:
          (cond
            [(and (has-uppercase? user-answer) (not(has-matching-parentheses? user-answer))) (displayln " \n ATENÇÃO -> A resposta contém letras maiúsculas\n ATENÇÃO -> A resposta contém erro de abertura e fechamento de parentêses!! \n")]
            [(has-uppercase? user-answer) (displayln " \n ATENÇÃO -> A resposta contém letras maiúsculas!! ")]
            [(not(has-matching-parentheses? user-answer))(displayln " \n ATENÇÃO -> A resposta contém erro de abertura e fechamento de parentêses!! ")])
          (incorrect-answer) ; Exibe a mensagem de resposta incorreta.
          (select-level player (player-difficulty player) (player-stage player)))))) ; Seleciona o mesmo nível novamente.





; --------------------------------------------------------
; Seção : UI
; --------------------------------------------------------

;; Void -> Void
;; Função que controla as seleções do menu, permitindo ao jogador escolher entre as ações disponíveis.
;; (define (menu-select) ...)
;
(define (menu-select)
  (display " -> ")
  (let ((choose (string->number (read-line))))
    (cond
      ((= choose 1) (initialize-create))
      ((= choose 2) (print-help))
      ((= choose 3) (print-credits))
      ((= choose 4) (exit-game))
      (else (menu-game)))))





;; Void -> Void
;; Exibe o menu principal do jogo.
;; (define (display-menu-game) ...)
;
(define (display-menu-game)
  (newline)
  (displayln "                  ╔════════════════════════════════════════════╗")
  (displayln "                  ║        Aventuras na Terra de Racket        ║")
  (displayln "                  ╚════════════════════════════════════════════╝")
  (displayln "                  ╔════════════════════════════════════════════╗")
  (displayln "                  ║                 1. Jogar                   ║")
  (displayln "                  ║                 2. Ajuda                   ║")
  (displayln "                  ║                 3. Créditos                ║")
  (displayln "                  ║                 4. Sair                    ║")
  (displayln "                  ╚════════════════════════════════════════════╝")
  (newline))





;; Void -> Void
;; Função do menu principal do jogo, exibindo opções para o jogador escolher.
;; (define (menu-game) ...)
;
(define (menu-game)
  (display-menu-game)
  (menu-select))




;; Void -> Void
;; Realiza a ação correspondente ao comando de ajuda inserido pelo usuário.
;; (define (help-switch) ...)
;
(define (help-switch)
  (display " -> ")
  (let ((x (read-line)))
    (cond
        [(string=? x "1") (explore-tutorials)]
        [(string=? x "2") (consult-tips)]
        [(string=? x "3") (read-documentation)]
        [(string=? x "4") (menu-game) ]
        [else (print-help) ])))





;; Void -> Void
;; Função que exibe o menu de ajuda, fornecendo opções relacionadas a tutoriais e dicas.
;; (define (print-help) ...)
;
(define (print-help)
  (newline)
  (displayln "                  ╔════════════════════════════════════════════╗ ")
  (displayln "                  ║          Bem-vindo ao menu de ajuda        ║ ")
  (displayln "                  ╚════════════════════════════════════════════╝ ")
  (newline)
  (displayln "   Você está em uma jornada para aprender Racket e se tornar um mestre programador!! ")
  (newline)
  (displayln "                  ╔════════════════════════════════════════════╗")
  (displayln "                  ║        1. Explorar Tutoriais               ║")
  (displayln "                  ║        2. Consultar Dicas do Mestre        ║")
  (displayln "                  ║        3. Ler Documentação Mágica          ║")
  (displayln "                  ║        4. Voltar à Aventura                ║")
  (displayln "                  ╚════════════════════════════════════════════╝")
  (newline)
  
  (help-switch))





;; Void -> Void
;; Exibe os tutoriais do jogo
;; (define (explore-tutorials) ...)
;
(define (explore-tutorials)
  (displayln " Você está explorando tutoriais para aprimorar suas habilidades em Racket.")
  (displayln " Siga os tutoriais e torne-se um mestre programador!")
  (displayln " 1. Voltar a aba de ajuda")
  (newline)
  (process-input print-help explore-tutorials 1))





;; Void -> Void
;; Exibe as dicas do jogo
;; (define (consult-tips) ...)
;
(define (consult-tips)
  (displayln " Você está consultando as sábias dicas do mestre programador.")
  (displayln " Dica 1: Sempre teste seu código.")
  (displayln " Dica 2: Leia a documentação.")
  (displayln " 1. Voltar a aba de ajuda")
  (newline)
  (process-input print-help consult-tips 1))





;; Void -> Void
;; Exibe a documentação do jogo
;; (define (read-documentation) ...)
;
(define (read-documentation)
  (newline)
  ; Expressões básicas
  (displayln " Você está lendo a documentação mágica para desvendar os segredos de Racket.")
  (displayln " Aqui está a seção sobre expressões básicas:")
  (newline)
  
  ; Números e operações aritméticas
  (displayln " Operações aritméticas:          ")
  (displayln "   Soma: (+ 5 3) => 8            ")    ; Soma
  (displayln "   Subtração: (- 10 4) => 6      ")    ; Subtração
  (displayln "   Multiplicação: (* 2 6) => 12  ")    ; Multiplicação
  (displayln "   Divisão: (/ 8 2) => 4         ")    ; Divisão
  (newline)

  ; Strings
  (displayln " Manipulação de strings:")
  (displayln "   Concatenação: (string-append \"Olá, \" \"mundo!\") => retorna a string a concantenada, com a string b. ") ; Concatenação
  (displayln "   Tamanho da string: (string-length ''Racket'') => retorna o tamanho da string.                          ") ; Tamanho da string
  (newline)

  ; Condicionais
  (displayln " Estruturas condicionais:")
  (displayln "   (define (verificar-paridade num)
       (if (even? num)
        ''Par''
        ''Ímpar''))")
  (displayln " Verificar paridade: (verificar-paridade 7) => ''Ímpar'' ")  ; Retorna "Ímpar"
  (newline)
  (displayln " 1. Voltar a ajuda")
  (newline)
  (process-input print-help read-documentation 1))





;; Void -> Void
;; Exibir os créditos do jogo
;; (define (display-credits) ...)
;
(define (display-credits)
  (newline)
  (displayln "                  ╔════════════════════════════════════════════╗")
  (displayln "                  ║            Bem Vindo aos créditos.         ║")
  (displayln "                  ╚════════════════════════════════════════════╝")
  (newline)
  (displayln "           Aventuras na terra Racket é um jogo educativo, desenvolvido
           para a matéria de Paradigma de Programação Lógica e Funcional
           do curso de ciência da computação fornecido pela Universidade
           Estadual de Maringá")
  (newline)
  (displayln "           A seguir, o nome e o RA de cada integrante envolvido no
           desenvolvimento")
  (newline)
  (displayln "                  ╔════════════════════════════════════════════╗ ")
  (displayln "                  ║  Andrei Roberto da Costa  ..... RA 107975  ║                         ")
  (displayln "                  ║  João Gilberto Casagrande ..... RA 112684  ║                         ")
  (displayln "                  ╚════════════════════════════════════════════╝ ")
  (newline)
  (displayln "           1. Retornar ao menu")
  (newline))





;; Void -> Void
;; Função que imprime os créditos do jogo, exibindo informações sobre o desenvolvimento e os integrantes da equipe.
;; (define (print-credits) ...)
;
(define (print-credits)
  (display-credits)
  (process-input menu-game print-credits 1))





;; Void -> Void
;; Exibe uma mensagem de boas-vindas e instruções para a seleção de um nome.
;; (display-select-name)
;
(define (display-select-name)
  (newline)
  (write " Bem-vindo(a) à Aba de Seleção de Nome, o ponto de partida para a sua jornada extraordinária!
 Neste reino digital, o nome que você escolher será mais do que uma simples etiqueta, será a
 personificação da sua jornada e conquistas.")
  (newline))





;; Void -> Void
;; Exibe uma mensagem de parabéns quando um usuário avança para um novo nível.
;; (define (notify-level-up level)
;; Parâmetros:
;;   - level: O nível para o qual o usuário avançou.
;
(define (notify-level-up level)
  (define border-up "              ╔═*══*══*══*══*══*══*══*══*══*══*══*══*══*══*═╗ ") ; String que representa uma linha de separação visual para destacar a mensagem
  (define border-down "              ╚═*══*══*══*══*══*══*══*══*══*══*══*══*══*══*═╝ ")
  (newline)
  (displayln border-up) ; Exibe a linha de separação superior.
  (displayln (format "              *   Parabéns!! Você avançou para o nível ~a    *" level)) ; Exibe a mensagem de parabéns com o nível alcançado.
  (displayln border-down) ; Exibe a linha de separação inferior.
  (newline))





; -------------------------------------------------------------------------------------------------------
; Estágios
; -------------------------------------------------------------------------------------------------------

(define level-1-easy 
  (level 1 
         " Início da Jornada" 
         " Você encontra-se em uma vila pacífica, onde os aldeões estão enfrentando um desafio. Uma criatura misteriosa deixou cair uma carta com a equação mágica escrita: 'Verifique se x é igual a 5'. Os anciãos da vila acreditam que resolver essa equação trará bênçãos e fortalecerá suas habilidades mágicas. Você aceita o desafio e prepara-se para iniciar sua jornada, enfrentando o primeiro enigma mágico." 
         "(= x 5)" 
         50 
         " Ao resolver a equação mágica 'Verifique se x é igual a 5', você sente uma onda de energia positiva fluindo através de você. Os anciãos da vila agradecem e reconhecem suas habilidades mágicas aprimoradas. Animado com o sucesso, você segue adiante na jornada, ansioso para enfrentar mais desafios que aprimorarão ainda mais suas habilidades em Racket."))

(define level-2-easy  
  (level 2 
         " A Floresta Encantada" 
         " Após resolver o primeiro enigma, você adentra uma floresta encantada. Aqui, as árvores sussurram enigmas e os riachos guardam segredos mágicos. Um elfo sábio apresenta-lhe o desafio: 'Some 3 ao valor de y'. Decifre esta expressão e prove sua astúcia para avançar na jornada." 
         "(+ y 3)" 
         50 
         " Ao desvendar a expressão mágica '(+ y 3)', as árvores da floresta murmuram em aprovação. O elfo sábio acena com a cabeça, reconhecendo sua habilidade em manipular expressões básicas em Racket. Com a confiança renovada, você segue mais profundamente na floresta, preparado para os desafios que estão por vir."))

(define level-3-easy  
  (level 3 
         " Dança dos Magos" 
         " Você emerge da floresta em direção a uma torre majestosa. Os magos que a habitam testarão suas habilidades com a magia das funções. Eles propõem o desafio: 'Some y ao valor de x'. Domine esta arte mágica para desbloquear o próximo nível de poder." 
         "(+ x y)" 
         100 
         " Ao dominar a magia de adicionar 'y' ao valor de 'x', os magos aplaudem sua perícia. Com confiança renovada, você avança para desafios mais complexos, sabendo que a jornada mágica está apenas começando."))



(define level-1-medium
  (level 4
         " Despertar da Magia"
         " Em um reino distante, onde a magia adormecida aguarda despertar, você é convocado para resolver o enigma mágico. A mensagem perdida revela: 'Defina uma função, chamada de 'increment', que retorna a soma de x com o valor 1'. Os sábios do reino acreditam que desvendar este mistério trará à tona poderes mágicos ocultos. Aceite o desafio e embarque na jornada mágica, enfrentando o primeiro enigma."
         "(define (increment x) (+ x 1))"
         100
         " Ao resolver o enigma, você sente uma corrente de energia mágica pulsando através de você. A paisagem ao seu redor parece vibrar com uma nova vitalidade, e os sábios, reconhecendo sua realização, sorriem em aprovação. Animado pelo sucesso, você se prepara para enfrentar os desafios subsequentes, ciente de que a magia despertada é apenas o começo de uma jornada incrível."))

(define level-2-medium
  (level 5
         " Travessia da Sombra Verde"
         " Após desvendar o enigma inicial, você se aventura através da Sombra Verde, uma floresta encantada envolta em mistérios. Árvores sussurram enigmas e riachos guardam segredos. Um guardião élfico apresenta o desafio: 'Defina uma função, chamada de 'double', que retorna o dobro de um valor 'x'. Decifre esta expressão e prove sua astúcia para avançar na jornada mágica."
         "(define (double x) (* x 2))"
         100
         " Ao decifrar a expressão mágica, a floresta responde com suspiros aprovativos. O guardião élfico, com um aceno de cabeça, indica que você está pronto para desafios mais profundos. Fortalecido pela experiência, você continua sua travessia, ansioso pelos enigmas que aguardam na sombra mágica da floresta."))

(define level-3-medium  
  (level 6 
         " Ascensão na Torre do Saber" 
         " Emergindo da densa floresta, sua jornada o leva em direção a uma majestosa Torre do Saber, um farol de conhecimento e magia. Os magos sábios que a habitam desejam testar suas habilidades nas complexas artes das funções. Eles apresentam o desafio: 'Defina uma função chamada 'square', que retorna a raiz quadrada de um valor x'. Domine esta arte mágica para desbloquear os próximos níveis de poder."
         "(define (square x)(* x x))" 
         200 
         " Ao aprimorar a magia de 'square', a torre ressoa com a vibração da magia aprimorada. Os magos reconhecem sua maestria e indicam o caminho para desafios ainda mais profundos e poderosos."))





(define level-1-hard
  (level 7
         "Portal para a Floresta Encantada"
         "Em meio a uma floresta mágica, você descobre um portal secreto para a enigmática Floresta Encantada. Os seres místicos que habitam este local mágico estão em apuros. Uma criatura alada deixou cair uma mensagem com a equação mágica escrita: 'Defina uma função, chamada de 'encantamento-especial' que combine a lista b à lista a usando a função 'cons' e, em seguida, aplique um encantamento especial na nova lista resultante.'. Acreditam que resolver este enigma trará equilíbrio à floresta e fortalecerá suas conexões com a natureza. Aceitando o desafio, você se prepara para embarcar nesta jornada, pronta para enfrentar o primeiro enigma mágico."
         "(define (encantamento-especial)(cons a b))"
         150
         " Ao realizar o encantamento especial, a floresta ressoa com alegria, e as criaturas mágicas te saúdam com gratidão. O portal para a próxima fase da sua jornada se abre, indicando o caminho para desafios mais profundos e misteriosos."))

(define level-2-hard
  (level 8
         "A Câmara dos Sussurros"
         "Do outro lado do portal, você entra na Câmara dos Sussurros, um lugar onde as vozes dos antigos sussurram segredos e desafios. Inscrições antigas revelam a próxima tarefa: 'Defina uma nova lista, chamada de 'nova-lista', onde os seus valores será a raiz quadrada (Utilize o square) dos valores da lista 'lista'. Para isso é necessário utilizar o comando 'map''. Os espíritos sussurrantes acreditam que superar este desafio abrirá portas para conhecimentos antigos e aprimorará sua habilidade mágica. Preparado para o desafio, você se aprofunda na Câmara dos Sussurros, ansioso pelo próximo enigma."
         "(define nova-lista (list (map square lista)))"
         150
         " Ao decifrar os sussurros antigos, uma aura de sabedoria o envolve. Portas secretas se revelam, indicando o caminho adiante em sua jornada mágica."))

(define level-3-hard
  (level 9
         "As Ruínas do Tempo"
         "Emergindo da Câmara dos Sussurros, você chega às Ruínas do Tempo, um local onde passado, presente e futuro se entrelaçam. Uma entidade temporal revela um novo desafio: 'Crie um feitiço temporal, chamado de 'feitiço-passado', onde realiza uma comparação do valor 'x', para caso ele seja 0, se for, nada acontece, caso contrário, chama o feitiço, diminuindo 1, cada vez que o feitiço for chamado..'. A entidade acredita que superar este desafio desvendará mistérios do tempo e aprimorará seus poderes dimensionais. Com coragem renovada, você aceita o desafio e adentra as Ruínas do Tempo, prontos para enfrentar o nono enigma."
         "(define (feitiço-passado x) (if (= x 0) 1 (feitiço-temporal (- x 1))))"
         300
         " Ao lançar o feitiço temporal, as ruínas ecoam com eco do passado e visões do futuro. Uma energia temporal pulsante indica que você desvendou mais um mistério, preparando-o para os desafios que aguardam à frente. O portal das Ruínas do Tempo se ilumina, sugerindo que a próxima fase de sua jornada está prestes a se revelar."))




;; ------------------------------------------ TESTES ------------------------------------------

;; Void -> Void
;; Executa todos os testes relacionados às funções do jogo, agrupando os testes de várias funções em um único local para facilitar a execução e verificação da validade do código.
;; (define (execut-all-tests) ...)
;
(define (execut-all-tests)
  (executa-testes (remove-space-tests))
  (executa-testes (has-uppercase?-tests))
  (executa-testes (has-matching-parentheses?-tests))
  (executa-testes (set-difficulty-tests))
  (executa-testes (verify-level-up-tests))
  (executa-testes (next-difficulty-tests))
  (executa-testes (verify-difficulty-input-tests)))

(execut-all-tests) ; Chama a execução de todos os testes

;; ------------------------------------------ TESTES ------------------------------------------



;; ------------------------------------------- GAME -------------------------------------------

(menu-game) ; Inicia o jogo

;; ------------------------------------------- GAME -------------------------------------------


