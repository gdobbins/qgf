// Copyright (C) 2019 by Graham Dobbins

package qgf

object card extends Enumeration {
  type card = Value
  val ace, two, three, four, five, six, seven, eight, nine, ten, jack, queen, king, unknown = Value
  val table = Map(
    "ace" -> ace,
    "two" -> two,
    "three" -> three,
    "four" -> four,
    "five" -> five,
    "six" -> six,
    "seven" -> seven,
    "eight" -> eight,
    "nine" -> nine,
    "ten" -> ten,
    "jack" -> jack,
    "queen" -> queen,
    "king" -> king
  )
}

import scala.collection.mutable.ArrayBuffer

class physcard {
  var cd = card.unknown
  val nots = new ArrayBuffer[card.card]
  def knownp = cd != card.unknown

  def mustbe(newcard: card.card) = {cd = newcard}

  def mustbep(newcard: card.card) = {cd == newcard}

  def cantbe(newcard: card.card) = {nots += newcard}

  def canbep(newcard: card.card) = {! nots.contains(newcard)}
}

class player(val name: String, val comp: Boolean = true) {
  val hand = ArrayBuffer[physcard](new physcard, new physcard, new physcard, new physcard, new physcard)
  def hasany(newcard: card.card) = {
    var ret = false
    for (c <- hand){
      if (c mustbep newcard) {ret = true}
    }
    ret
  }
  def askfor(newcard: card.card) = {
    val ret = new ArrayBuffer[physcard](0)
    if (hasany(newcard)) {
      for (c <- hand){
        if (c mustbep newcard) {
          ret += c
        }
      }
      hand --= ret
      println(s"${name} gives ${ret.length} ${newcard}s")
      for (c <- hand){
        c cantbe newcard
      }
      ret
    }
    else {
      ret += new physcard
      for (c <- hand){
        c cantbe newcard
      }
      println(s"${name} says Go Fish!")
      ret
    }
  }
  def take(other: player, newcard: card.card) = {
    var have = false
    for (c <- hand) {
      if (c mustbep newcard) { have = true }
    }
    if (! have) {
      for (c <- hand) {
        if (c canbep newcard) {
          if (! have){
            c mustbe newcard
            have = true}
        }
      }
    }
    if (! have) { throw new IllegalStateException("Must have card to ask for it!") }
    for (c <- other.askfor(newcard)){
      hand += c
    }
  }

  def checkwin = {
    var count = 0
    var win = false
    for(c1 <- hand if c1 knownp){
      for(c2 <- hand){
        if (c1.cd == c2.cd) count += 1
      }
      if (count >= 4) win = true
      else count = 0
    }
    win
  }
}

import scala.io.StdIn.readLine
import java.util.Random

object qgf {
  var done = false
  val rand = new Random(System.currentTimeMillis())

  def human_turn(person: player, others: Array[player]): Int = {
    val prompt = s"${person.name}: "
    var names = ""
    for (p <- others){
      names += p.name
      names += ", "
    }
    names = names.dropRight(2)
    print(prompt + s"Who do you ask? [" + names + "]: ")
    val who = readLine()
    if (who == "quit"){
      done = true
      return 0
    }
    var whop = person
    for (p <- others){
      if (who == p.name){
        whop = p
      }
      if (who == p.name.drop(3)){
        whop = p
      }
    }
    if (whop == person){
      throw new IllegalStateException("Not a valid player name!")
    }
    print("\n" + prompt + "What do you ask for? ")
    val what = readLine()
    val whatc = card.table(what)
    person.take(whop, whatc)
    if (person checkwin) {
      println(s"${person.name} wins!")
      done = true
    }
    0
  }

  def computer_turn(person: player, others: Array[player]): Int = {
    val prompt = s"${person.name}: "
    var names = ""
    for (p <- others){
      names += p.name
      names += ", "
    }
    names = names.dropRight(2)
    print(prompt + s"Who do you ask? [" + names + "]: ")
    val rand_index = rand.nextInt(others.length)
    val who = others(rand_index).name
    print(who)
    var whop = person
    for (p <- others){
      if (who == p.name){
        whop = p
      }
    }
    if (whop == person){
      throw new IllegalStateException("Not a valid player name!")
    }
    print("\n" + prompt + "What do you ask for? ")
    val rand_index_2 = rand.nextInt(13)
    val whatc = card(rand_index_2)
    println(whatc)
    person.take(whop, whatc)
    if (person checkwin) {
      println(s"${person.name} wins!")
      done = true
    }
    0
  }

  def turn(person: player, others: Array[player]) = {
    if (person.comp){
      computer_turn(person, others)
    }
    else{
      human_turn(person, others)
    }
  }

  def main(args: Array[String]): Unit = {
    println("Welcome to Quantum Go Fish!")
    val human = new player("Player", false)
    val comp0 = new player("AI 0")
    val comp1 = new player("AI 1")
    val comp2 = new player("AI 2")
    var p1 = human
    val oths = Array(comp0, comp1, comp2)
    var swap = human
    while(! done){
      try{
        turn(p1, oths)
        swap = p1
        p1 = oths(0)
        oths(0) = oths(1)
        oths(1) = oths(2)
        oths(2) = swap
      } catch {
        case e: Exception => println(e)
      }
    }
  }
}
