(ns hangman
	(:gen-class)
	(:require [clojure.string :only [join upper-case blank]]))

(def alphaSet
	"Possible guess letters for Hangman in uppercase."
	(map char (range (int \A) (inc (int \Z)))))

(def maxGuesses
	"Maximum guess count for Hangman."
	6)

(defn wordList
	"Generate list of possible words as stored in passed file.
     One word per line in file.  Words are forced to upper case."
    [filename]
    (with-open [rdr (clojure.java.io/reader filename)]
        (map clojure.string/upper-case (doall (line-seq rdr)))))

(defn randomWord
	"Random item from passed, non-empty, list."
	[words]
	(rand-nth words))

(defn wordJoin
	"Join the list of characters together with a space in-between."
	[letters]
	(clojure.string/join  " " letters))

(defn in?
 	"True if element is present in list."
  	[coll element]
  	(some #(= element %) coll))

(defn applyGuess
	"Generate a new guess list based on letter, current matches, and actual word."
	[letter guesslist hanglist]
	(clojure.string/join (map #(if (= letter %2) %2 %1) guesslist hanglist)))

(defn applyGuess'
	"Generate a new guess set based on letter, current set, and actual word."
	[letter guess_set hanglist]
	(cond
		(not (in? alphaSet  letter)) guess_set
		     (in? guess_set letter)  guess_set
		(not (in? hanglist  letter)) (conj guess_set letter)
		:else guess_set
		))

(defn padLeft
	"Pad left side of string with spaces."
	[width text]
	(if (<= width (count text)) text (padLeft width (str " " text))))

(defn formatSummary
	"Output message for wins and losses."
	[message wins losses]
	(print   (str message "  "))
	(print   (str "Wins : " (padLeft 2 (str wins)) " "))
	(println (str "Losses : " (padLeft 2 (str losses)))))

(defn formatInput
	"Output message for new letter."
	[message remaining]
	(print (str "  " message "  "))
	(print (str "[Guesses left : " (padLeft 2 (str remaining)) " ] "))
	(print (str "Letter : "))
	(flush)	
	)

(defn formatWord
	"Output Hangman word."
	[hanglist]
	(println (str "  " (wordJoin hanglist)))
	(println))

(defn playRound
	"Play a single round (one-word) of Hangman."
	[hanglist guesslist guess_set]


	(cond
		(= hanglist guesslist)
			(do
				(formatWord hanglist)
				[:NewGame :Win])

		(<= maxGuesses (count guess_set))
			(do
				(formatWord hanglist)
				[:NewGame :Loss])

		:else
			(do
				(formatInput (wordJoin guesslist) (- maxGuesses (count guess_set)))
				(def guess (clojure.string/upper-case (read-line)))
				(def exitResult (if (= 0 (count guess_set)) :Quit :Loss))
				(cond
					(clojure.string/blank? guess) (playRound hanglist guesslist guess_set)
					(= "EXIT" guess)
						(do
							(formatWord hanglist)
							[:Exit exitResult])
					(= "NEW"  guess)
						(do
							(formatWord hanglist)
							[:NewGame exitResult])
					:else
						(do
							(def new_guesslist (applyGuess  (first guess) guesslist hanglist))
							(def new_guess_set (applyGuess' (first guess) guess_set hanglist))
							(playRound hanglist new_guesslist new_guess_set))
				)
			)
		)
	)

(defn playGame
	"Play the game of Hangman."
	[wins losses words]

	
    (println "Type 'Exit' to leave the game, 'New' for a new game.")
    (println "Good luck!\n")

    
    (def hanglist (randomWord words))

    
    (def guesslist (apply str (repeat (count hanglist) "_")))
    (def guess_set #{})

    
    (let [[action' result'] (playRound hanglist guesslist guess_set)]
	(def action action')	
	(def result result'))	

   	(def new_wins   (if (= :Win  result) (+ 1 wins  ) wins))
    (def new_losses (if (= :Loss result) (+ 1 losses) losses))
	(def new_words (filter #(not= hanglist %) words))

	(cond
		(= :Win  result) (formatSummary "Congratulations!" new_wins new_losses)
		(= :Loss result) (formatSummary "Too Bad!  Please try again." new_wins new_losses)
		:else nil
		)

	(cond
		(= :NewGame action) (playGame new_wins new_losses new_words)
		(= :Exit    action) (formatSummary "Thank you for playing!" new_wins new_losses)
		:else nil
		)
   	)

(defn -main
  	"The Hangman Game"

  	([] (-main "C:/Users/mohan/Desktop/test.txt"))
  	([fname]
  	
  	(println "Welcome to the Hangman word guessing game.")
	
  	(def words (wordList fname))

    (playGame 0 0 words))
    )
