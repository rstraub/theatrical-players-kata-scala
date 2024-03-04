package nl.codecraftr.scala.kata

import java.lang.System.lineSeparator
import java.text.NumberFormat
import java.util.Locale

final case class Invoice(customer: String, performances: List[Performance]) {

  def totalCostCalculation(
      plays: Map[String, Play]
  ): Int = {
    var totalAmount = 0
    for (perf <- performances) {
      val thisAmount = plays(perf.playId).calculateCosts(perf)
      totalAmount += thisAmount
    }
    totalAmount
  }

}

final case class Performance(playId: String, audience: Int){
}
final case class Play(name: String, `type`: String){
  // what do you use of the performance?
     def calculateCosts(perf: Performance): Int = {
        `type` match {
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
            case _ => throw new Exception("unknown type: " + `type`)
        }
    }
}

class StatementPrinter {
  private val culture = Locale.US
  def print(invoice: Invoice, plays: Map[String, Play]): String =
    header(invoice.customer) + createBody(invoice, plays) + createFooter(
      invoice,
      plays
    )

  private def createBody(invoice: Invoice, plays: Map[String, Play]): String = {
    var result: String = ""
    for (perf <- invoice.performances) {
      result += bodyLine(plays(perf.playId), perf)
    }
    result
  }

  private def calculateTotalVolumeCredits(
      invoice: Invoice,
      plays: Map[String, Play]
  ) = {
    var volumeCredits = 0
    for (perf <- invoice.performances) {
      val play = plays(perf.playId)

      volumeCredits += calculateVolumeCredits(play, perf)
    }
    volumeCredits
  }

  private def bodyLine(play: Play, perf: Performance): String = {
    val amount = play.calculateCosts(perf)
    s"  ${play.name}: ${NumberFormat
        .getCurrencyInstance(culture)
        .format((amount / 100).toDouble)} (${perf.audience} seats)$lineSeparator"
  }

  private def header(customer: String): String =
    s"Statement for ${customer}$lineSeparator"

  private def createFooter(
      invoice: Invoice,
      plays: Map[String, Play]
  ): String = {
    val totalAmount = invoice.totalCostCalculation(plays)
    val volumeCredits = calculateTotalVolumeCredits(invoice, plays)
    var footer =
      s"Amount owed is ${NumberFormat.getCurrencyInstance(culture).format(totalAmount / 100d)}$lineSeparator"
    footer += s"You earned ${volumeCredits} credits$lineSeparator"
    footer
  }

  private def calculateVolumeCredits(play: Play, perf: Performance): Int = {
    // add volume credits
    var volumeCredits = Math.max(perf.audience - 30, 0)
    // add extra credit for every ten comedy attendees
    if ("comedy" == play.`type`)
      volumeCredits += Math.floor(perf.audience / 5d).toInt
    volumeCredits
  }

}
