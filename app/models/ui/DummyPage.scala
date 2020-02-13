/*
 * Copyright 2020 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package models.ui


object DummyPage {
  val title = Text("Telling HMRC about extra income",
                   "Tudalen Arddangos Yn Adrodd HMRC am incwm ychwanegol")

  val openingPara = Text("Check if you need to tell HMRC about extra money you’ve made by selling goods or services, or renting land or property.",
                         "Gwiriwch a oes angen i chi ddweud wrth HMRC am arian ychwanegol rydych chi " +
                         "wedi'i wneud trwy werthu nwyddau neu wasanaethau, neu rentu tir neu eiddo.")

  val overView = Text("Overview", "Trosolwg")

  val bulletLeading1 = Text("In some circumstances, you do not have to tell HMRC about extra income you’ve made. " +
                            "In each tax year you can earn up to £1,000, tax free, if you are:",
                            "Mewn rhai amgylchiadau, nid oes rhaid i chi ddweud wrth HMRC am incwm ychwanegol rydych chi wedi'i wneud. " +
                            "Ymhob blwyddyn dreth gallwch ennill hyd at £ 1,000, yn ddi-dreth, os ydych chi:")

  val bulletOpt1a = Text("selling goods or services (trading)", "gwerthu nwyddau neu wasanaethau (masnachu)")
  val bulletOpt1b = Text("renting land or property", "rhentu tir neu eiddo")

  val para1 = Text("A tax year runs from 6 April one year to 5 April the next.",
                   "Mae blwyddyn dreth yn rhedeg rhwng 6 Ebrill un flwyddyn a 5 Ebrill y flwyddyn nesaf.")

  val heading2 = Text("I’ve made extra income from selling goods or services",
                      "Rwyf wedi gwneud incwm ychwanegol o werthu nwyddau neu wasanaethau")

  val para2 = Text("This can include selling items or offering freelance services. If you make extra money in this way, you’re likely to be trading.",
                   "Gall hyn gynnwys gwerthu eitemau neu gynnig gwasanaethau ar eu liwt eu hunain. " +
                   "Os gwnewch arian ychwanegol fel hyn, rydych yn debygol o fod yn masnachu.")
  val para3 = Text("Find out more about ", "Darganfyddwch fwy am ")
  val eLink3 = Text("how HMRC decides if you are trading or not.", "sut mae HMRC yn penderfynu a ydych chi'n masnachu ai peidio.")

  val para4 = Text("If you’ve only sold personal possessions then you’re probably not trading. " +
                   "You will not have to pay income tax on the money you make, but you might have to pay ",
                   "Os mai dim ond eiddo personol rydych chi wedi'i werthu yna mae'n debyg nad ydych chi'n masnachu. " +
                   "Ni fydd yn rhaid i chi dalu treth incwm ar yr arian a wnewch, ond efallai y bydd yn rhaid i chi dalu ")
  val eLink4 = Text("Capital Gains Tax.","Treth Enillion Cyfalaf.")

  val tradingAllowance = Text("The trading allowance", "Y lwfans masnachu")

  val bulletLeading2 = Text("The trading allowance lets you earn up to £1,000 from any trading, " +
                            "casual or miscellaneous income, tax free, in each tax year. For example:",
                            "Mae'r lwfans masnachu yn caniatáu ichi ennill hyd at £ 1,000 o unrhyw incwm masnachu, " +
                            "achlysurol neu amrywiol, yn ddi-dreth, ym mhob blwyddyn dreth. Er enghraifft:")
  val bulletOpt2a = Text("selling items online or face to face", "gwerthu eitemau ar-lein neu wyneb yn wyneb")
  val bulletOpt2b = Text("selling freelance services (such as gardening or babysitting)",
                         "gwerthu gwasanaethau ar eu liwt eu hunain (fel garddio neu warchod plant)")
  val bulletOpt2c = Text("hiring out personal equipment (such as power tools)", "llogi offer personol (fel offer pŵer)")

  val heading3 = Text("I’ve made extra income from renting land or property", "Rwyf wedi gwneud incwm ychwanegol o rentu tir neu eiddo")

  val para5 = Text("Property income can include any money you earn by renting land or buildings.",
                   "Gall incwm eiddo gynnwys unrhyw arian rydych chi'n ei ennill trwy rentu tir neu adeiladau.")
  val propertyAllowance = Text("The property allowance", "Y lwfans eiddo")

  val bulletLeading3 = Text("The property allowance lets you earn up to £1,000 in rental income, tax free, in each tax year. For example:",
                            "")
  val bulletOpt3a = Text("renting a flat or house", "")
  val bulletOpt3b = Text("renting out a room in your home", "")
  val bulletOpt3c = Text("short term holiday lets", "")
  val bulletOpt3d = Text("renting out a parking space or garage", "")

  val para6 = Text("A tax year runs from 6 April one year to 5 April the next.", "")


  val link1 = Text("Check if you need to tell HMRC about your extra income",
                   "Gwiriwch a oes angen i chi ddweud wrth HMRC am eich incwm ychwanegol")
  val link2 = Text("Check if you need to tell HMRC about income you've made by selling goods or services",
                   "Gwiriwch a oes angen i chi ddweud wrth HMRC am incwm rydych chi wedi'i wneud trwy werthu nwyddau neu wasanaethau")
  val link3 = Text("Check if you need to tell HMRC about income you've made by renting land or property",
                   "Gwiriwch a oes angen i chi ddweud wrth HMRC am incwm rydych wedi'i wneud trwy rentu tir neu eiddo")

  val pageItems: Seq[UIComponent] = Seq(
                    H1(title),
                    Paragraph(Seq(openingPara), lede = true),
                    H2(overView),
                    BulletPointList(Seq(bulletLeading1),
                                    Seq(Seq(bulletOpt1a), Seq(bulletOpt1b))),
                    Paragraph(Seq(para1)),
                    HyperLink( "/how-did-you-earn-extra-income", link1),
                    H2(heading2),
                    Paragraph(Seq(para2)),
                    Paragraph(Seq(para3, HyperLink("https://www.youtube.com/watch?v=MYgCctGY_Ug", eLink3))),
                    Paragraph(Seq(para4, HyperLink("https://www.gov.uk/capital-gainstax", eLink4))),
                    H3(tradingAllowance),
                    BulletPointList(Seq(bulletLeading2),
                                    Seq(Seq(bulletOpt2a), Seq(bulletOpt2b), Seq(bulletOpt2c))),
                    Paragraph(Seq(para1)),
                    HyperLink( "/sales/did-you-only-sell-personal-posessions", link2),
                    H2(heading3),
                    Paragraph(Seq(para5)),
                    H3(propertyAllowance),
                    BulletPointList(Seq(bulletLeading3),
                                    Seq(Seq(bulletOpt3a), Seq(bulletOpt3b), Seq(bulletOpt3c), Seq(bulletOpt3d))),
                    Paragraph(Seq(para1)),
                    HyperLink( "/rent/have-you-made-less-than-1000", link3)
                  )
    val page = Page("dummy-path", pageItems)
}
