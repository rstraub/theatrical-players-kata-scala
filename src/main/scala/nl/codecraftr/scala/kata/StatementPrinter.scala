package nl.codecraftr.scala.kata

import java.lang.System.lineSeparator
import java.text.NumberFormat
import java.util.Locale

final case class Invoice(customer: String, performances: List[Performance])
final case class Performance(playId: String, audience: Int)
final case class Play(name: String, `type`: String)

class StatementPrinter {
  // Misleading name? Doing more than printing
  // Violating Separation of concern
  def print(invoice: Invoice, plays: Map[String, Play]): String = {
    var totalAmount = 0
    var volumeCredits = 0

    // Formatting for text statement
    var result = s"Statement for ${invoice.customer}$lineSeparator"

    val culture = Locale.US

    for (perf <- invoice.performances) {
      val play = plays(perf.playId)
      val thisAmount = calculateCosts(play, perf)

      volumeCredits += calculateVolumeCredits(play, perf)

      // print line for this order
      // Formatting for text statement
      result += s"  ${play.name}: ${NumberFormat
          .getCurrencyInstance(culture)
          .format((thisAmount / 100).toDouble)} (${perf.audience} seats)$lineSeparator"

      // CALCULATE TOTAL AMOUNT
      totalAmount += thisAmount;
    }

    // Formatting for text statement
    result += s"Amount owed is ${NumberFormat.getCurrencyInstance(culture).format(totalAmount / 100d)}$lineSeparator"
    result += s"You earned ${volumeCredits} credits$lineSeparator"

    result
  }

  def calculateVolumeCredits(play: Play, perf: Performance): Int = {
    // add volume credits
    var volumeCredits = Math.max(perf.audience - 30, 0)
    // add extra credit for every ten comedy attendees
    if ("comedy" == play.`type`)
      volumeCredits += Math.floor(perf.audience / 5d).toInt
    volumeCredits
  }

  def calculateCosts(play: Play, perf: Performance): Int = {
    play.`type` match {
      case "tragedy" => {
        val creditThresholdTragedy = 30
        var thisAmount = 40000
        if (perf.audience > creditThresholdTragedy)
          thisAmount += 1000 * (perf.audience - creditThresholdTragedy)
        thisAmount
      }
      case "comedy" => {
        val creditThresholdComedy = 20
        var thisAmount = 30000
        if (perf.audience > creditThresholdComedy)
          thisAmount += 10000 + 500 * (perf.audience - creditThresholdComedy)
        thisAmount += 300 * perf.audience
        thisAmount
      }
      case _ => throw new Exception("unknown type: " + play.`type`)
    }
  }
}
