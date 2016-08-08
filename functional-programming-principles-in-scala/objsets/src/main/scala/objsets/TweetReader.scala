// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package objsets

object TweetReader {

  object ParseTweets {

    import scala.util.parsing.json._

    def getList[T](s: String): List[T] =
      JSON.parseFull(s).get.asInstanceOf[List[T]]

    def getMap(s: String): Map[String, Any] =
      JSON.parseFull(s).get.asInstanceOf[Map[String, Any]]

    def getTweets(user: String, json: String): List[Tweet] =
      for (map <- getList[Map[String, Any]](json)) yield {
        val text = map("text")
        val retweets = map("retweet_count")
        new Tweet(user, text.toString, retweets.toString.toDouble.toInt)
      }

    def getTweetData(user: String, json: String): List[Tweet] = {
      // is list
      val l = getList[Map[String, Any]](json)
      for (map <- l) yield {
        val text = map("text")
        val retweets = map("retweets")
        new Tweet(user, text.toString, retweets.toString.toDouble.toInt)
      }
    }
  }

  def toTweetSet(l: List[Tweet]): TweetSet = {
    l.foldLeft(new Empty: TweetSet)(_.incl(_))
  }

  def unparseToData(tws: List[Tweet]): String = {
    val buf = new StringBuffer
    for (tw <- tws) {
      val json = "{ \"user\": \"" + tw.user + "\", \"text\": \"" +
        tw.text.replaceAll(""""""", "\\\\\\\"") + "\", \"retweets\": " +
        tw.retweets + ".0 }"
      buf.append(json + ",\n")
    }
    buf.toString
  }

  val sites: List[String] = List("gizmodo", "TechCrunch", "engadget", "amazondeals", "CNET", "gadgetlab", "mashable")

  private val gizmodoTweets: List[Tweet] = TweetReader.ParseTweets.getTweetData("gizmodo", TweetData.gizmodo)
  private val techCrunchTweets: List[Tweet] = TweetReader.ParseTweets.getTweetData("TechCrunch", TweetData.TechCrunch)
  private val engadgetTweets: List[Tweet] = TweetReader.ParseTweets.getTweetData("engadget", TweetData.engadget)
  private val amazondealsTweets: List[Tweet] = TweetReader.ParseTweets.getTweetData("amazondeals", TweetData.amazondeals)
  private val cnetTweets: List[Tweet] = TweetReader.ParseTweets.getTweetData("CNET", TweetData.CNET)
  private val gadgetlabTweets: List[Tweet] = TweetReader.ParseTweets.getTweetData("gadgetlab", TweetData.gadgetlab)
  private val mashableTweets: List[Tweet] = TweetReader.ParseTweets.getTweetData("mashable", TweetData.mashable)

  private val sources: List[List[Tweet]] = List(gizmodoTweets, techCrunchTweets, engadgetTweets, amazondealsTweets, cnetTweets, gadgetlabTweets, mashableTweets)

  val tweetMap: Map[String, List[Tweet]] =
    Map() ++ Seq(sites.head -> gizmodoTweets,
      sites(1) -> techCrunchTweets,
      sites(2) -> engadgetTweets,
      sites(3) -> amazondealsTweets,
      sites(4) -> cnetTweets,
      sites(5) -> gadgetlabTweets,
      sites(6) -> mashableTweets)

  val tweetSets: List[TweetSet] = sources.map(tweets => toTweetSet(tweets))

  private val siteTweetSetMap: Map[String, TweetSet] =
    Map() ++ (sites zip tweetSets)

  private def unionOfAllTweetSets(curSets: List[TweetSet], acc: TweetSet): TweetSet = {
    if (curSets.isEmpty) {
      acc
    } else {
      unionOfAllTweetSets(curSets.tail, acc.union(curSets.head))
    }
  }

  val allTweets: TweetSet = unionOfAllTweetSets(tweetSets, new Empty)
}
