package forcomp

import scala.io.{ Codec, Source }

object Anagrams extends AnagramsInterface:

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */

  /**
   * Groups the characters of a word. The key is the char in lowercase and the value is a string made up of only that
   * char, with a length according to the char occurrences. Then we just need to map from (char, string) to (char, int)
   * by getting the length of the string, convert to list and sort by the characters. E.g. banana =>
   * (b -> b, a -> aaa, n -> nn) => (b -> 1, a -> 3, n -> 2) => [(b, 1), (a, 3), (n, 2)] => [(a, 3), (b, 1), (n, 2)]
   */
  def wordOccurrences(w: Word): Occurrences =
    w.groupBy(char => char.toLower).map((char, string) => (char, string.length)).toList.sorted

  /** Converts a sentence into its character occurrence list. */

  /**
   * We can compute the sentence occurrences if we concatenate each word into a single string and then reuse the
   * previous function. If we get a null sentence then we just return null
   */
  def sentenceOccurrences(s: Sentence): Occurrences = s match {
    case Nil => Nil
    case _ => wordOccurrences(s.reduce((w1, w2) => w1 + w2))
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(word => wordOccurrences(word))

  /** Returns all the anagrams of a given word. */

  /**
   * If we have the dictionary whose keys are occurrences and values are lists of words, then we just need to get the
   * occurrence of a given word and use that to retrieve the anagrams for that word from the dictionary
   */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  /**
   * To get the list of all subsets of an occurrences we need three helper functions.
   *
   * powerSet - From wikipedia: "The power set (or powerset) of a set S is the set of all subsets of S, including the
   * empty set and S itself". This function is not mine but computing the power set of a given occurrences is
   * practically what we're trying to do. In this case let's treat occurrences as a set whose elements are (Char, Int).
   * Say we are given this occurrences [(a, 2), (b, 2)], its power set would have 4 elements:
   * 1. []
   * 2. [(a, 2)]
   * 3. [(b, 2)]
   * 4. [(a, 2), (b, 2)]
   * which is good as starting point, but for our problem we have the additional condition that for example
   * [(a, 1), (b, 2)] counts as a valid subset. So we'll need to use the other functions to do this extra logic.
   *
   * getSubsetsOfASingleOccurrence - A single occurrence is the pair (char, count) for a single char. For example, if we
   * have the occurrence (a, 2) then this function will return [[(a, 1)], [(a, 2)]] which themselves are
   * subsets of both the given occurrence and occurrences. It needs to return List[Occurrences] so that the it can be
   * combined with other lists.
   *
   * getSubsetsOfOccurrences - Each subset belonging to the power set will be passed to this function. With the given
   * occurrences what we do is check whether the occurrences is null, has only a single occurrence or it has more than
   * one. If it's null then just return a list of null. If it has only a single occurrence then we call the
   * getSubsetsOfASingleOccurrence function. If it has more than one then we get the subsets for the head and we'll
   * combine each subset recursively with each of the subsets for the rest of the occurrences. Using [(a, 2), (b, 2)]
   * as example the process would be something like this:
   *
   * occurrences: [(a, 2), (b, 2)]
   * head: (a, 2)
   * tail: [(b ,2)]
   * getSubsetsOfASingleOccurrence(head): [(a, 1), (a, 2)]
   * getSubsetsOfOccurrences(tail): [(b, 1), (b, 2)]
   * Use the for comprehension to calculate the cartesian product for the previous subsets
   * [(a, 1), (b, 1)]
   * [(a, 1), (b, 2)]
   * [(a, 2), (b, 1)]
   * [(a, 2), (b, 2)]
   *
   * Putting everything together we would get the following:
   * 1. [] => [[]]
   * 2. [(a, 2)] => [
   *      [(a, 1)]
   *      [(a, 2)]
   *    ]
   * 3. [(b, 2)] => [
   *      [(b, 1)]
   *      [(b, 2)]
   *    ]
   * 4. [(a, 2), (b, 2)] => [
   *      [(a, 1), (b, 1)]
   *      [(a, 1), (b, 2)]
   *      [(a, 2), (b, 1)]
   *      [(a, 2), (b, 2)]
   *    ]
   *
   * These four elements are put into another list in order to merge them, but this list is unnecessary so we flatten it
   * and thus the final result is the one stated at line 91
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {

    def getSubsetsOfASingleOccurrence(occurrence: (Char, Int)): List[Occurrences] =
       val (char, count) = occurrence
       for i <- (1 to count).toList yield List((char, i))

    def getSubsetsOfOccurrences(occurrences: Occurrences): List[Occurrences] = occurrences match {
      case Nil => List(Nil)
      case head :: Nil => getSubsetsOfASingleOccurrence(head)
      case head :: tail =>
        for
          subsetFromHead <- getSubsetsOfASingleOccurrence(head)
          subsetFromTail <- getSubsetsOfOccurrences(tail)
        yield subsetFromHead ::: subsetFromTail
    }

    def powerSet(occurrences: Occurrences): List[Occurrences] =
      occurrences.foldLeft( List( List[(Char, Int)]() ) ) { (sets, set) =>
        sets ++ sets.map(_ :+ set)
      }

    val occurrencesPowerSet = powerSet(occurrences)

    val subsets = for occurrences <- occurrencesPowerSet yield getSubsetsOfOccurrences(occurrences)
    subsets.flatten
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  /**
   * foldLeft is like a reduce operation but the difference is that the accumulator doesn't need to be of the same type
   * as the collection and we can provide an initial value for it. In this case the initial value is x converted to a
   * map and the accumulator is therefore of type map. As we iterate through y we get the key which is the char that we
   * want to subtract and the value which is the count of said char. We use the char to find the original count from the
   * map. If the result of the subtraction would be 0 then we remove the char from the map and if the result wouldn't be
   * 0 then we do the subtraction and update the value. We convert back to a list and sort it
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences =
    val newOccurrences = y.foldLeft(x.toMap) { (map, pair) =>
      val keyToSubtract = pair._1
      val countToSubtract = pair._2
      val originalCount = map(keyToSubtract)
      if countToSubtract == originalCount then map - keyToSubtract
      else map + (keyToSubtract -> (originalCount - countToSubtract))
    }
    newOccurrences.toList.sorted


  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my), 1
   *      List(en, my, as), 2
   *      List(man, yes), 3
   *      List(men, say), 4
   *      List(as, en, my), 5
   *      List(as, my, en), 6
   *      List(sane, my), 7
   *      List(Sean, my), 7
   *      List(my, en, as), 8
   *      List(my, as, en), 9
   *      List(my, sane), 10
   *      List(my, Sean), 10
   *      List(say, men), 11
   *      List(yes, man) 12
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */

  /**
   * A solution is a list of occurrences where each one of them can form a word and thus they together can form a
   * sentence
   */
  type Solution = List[Occurrences]

  /**
   * Since each solution is a list of occurrences, those occurrences can form multiple words
   */
  type PossibleWords = List[Word]

  /**
   * We use the sentenceOccurrences function to get the occurrences for the sentence.
   * This occurrences is then passed to getAllSolutionsForOccurrences.
   * 
   * getAllSolutionsForOccurrences - The general idea is that we get all the subsets for such occurrences and then we
   * iterate over every one of the subsets. If a subset can form a word(s) then we recursively build a solution by
   * subtracting that subset from our given occurrences to get the remaining occurrences, adding it to our solution acc
   * and repeating the process from the remaining. Once we get to the null case we have completed a solution and just
   * return the acc. We flatten the solutions at the end
   *
   * createSentences - As the name implies, this function will create the sentences from the given solutions obtained
   * from the previous function. We first need to use the dictionary to map the occurrences to the words it can form and
   * then we can use the helper combineWords function to form the sentences by computing the cartesian product of
   * PossibleWords. Let's assume we had a sentence like "Yes man I love you" and that this is valid:
   *
   * [as, en, my] <- This is of type PossibleWords
   * [you, olive] <- This is of type PossibleWords
   * Those two PossibleWords are in the list we pass to combineWords and we get the following:
   * [
   *   [as, you]
   *   [as, olive]
   *   [en, you]
   *   [en, olive]
   *   [my, you]
   *   [my, olive]
   * ]
   * At the end we just flatten
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] =

    def getAllSolutionsForOccurrences(occurrences: Occurrences, acc: Solution): List[Solution] =
        occurrences match {
      case Nil => List(acc)
      case _ =>
        val subsets = combinations(occurrences)
        val solutions = for
          currentOccurrences <- subsets if dictionaryByOccurrences.contains(currentOccurrences)
        yield {
          val remainingOccurrences = subtract(occurrences, currentOccurrences)
          val updatedAcc = acc :+ currentOccurrences
          getAllSolutionsForOccurrences(remainingOccurrences, updatedAcc)
        }
        solutions.flatten
    }

    def createSentences(solutions: List[Solution]): List[Sentence] =

      def combineWords(possibleCombinationsOfWords: List[PossibleWords]): List[Sentence] =
          possibleCombinationsOfWords match {
        case Nil => List(Nil)
        case wordsFromHead :: tail =>
          for
            wordFromHead <- wordsFromHead
            wordsFromTail <- combineWords(tail)
          yield wordFromHead +: wordsFromTail
      }

      val possibleSolutions = solutions.map(solution => solution.map(occurrences =>
        dictionaryByOccurrences(occurrences)))
      val anagrams = for possibleCombinationsOfWords <- possibleSolutions yield combineWords(possibleCombinationsOfWords)
      anagrams.flatten

    val occurrences = sentenceOccurrences(sentence)
    val solutions = getAllSolutionsForOccurrences(occurrences, List())
    createSentences(solutions)

object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()
