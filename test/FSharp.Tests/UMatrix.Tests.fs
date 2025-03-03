module UMatrixTests

open UMatrix

module Tests =
    open Expecto
    open Expecto.Flip
    open FsToolkit.ErrorHandling

    [<Tests>]
    let specs =
        ftestList
            "uMatrix"
            [ // theory data
              let validRuleTheoryData =
                  [ "* * * block",
                    { Source = Source.Any
                      Destination = Destination.Any
                      RequestType = RequestType.Any
                      Action = Action.Block }
                    "* * cookie block",
                    { Source = Source.Any
                      Destination = Destination.Any
                      RequestType = RequestType.Cookie
                      Action = Action.Block }
                    "* * css allow",
                    { Source = Source.Any
                      Destination = Destination.Any
                      RequestType = RequestType.Css
                      Action = Action.Allow }
                    "* * image allow",
                    { Source = Source.Any
                      Destination = Destination.Any
                      RequestType = RequestType.Image
                      Action = Action.Allow }
                    "* * frame block",
                    { Source = Source.Any
                      Destination = Destination.Any
                      RequestType = RequestType.Frame
                      Action = Action.Block }
                    "* * other block",
                    { Source = Source.Any
                      Destination = Destination.Any
                      RequestType = RequestType.Other
                      Action = Action.Block }
                    "* 1st-party * allow",
                    { Source = Source.Any
                      Destination = Destination.FirstParty
                      RequestType = RequestType.Any
                      Action = Action.Allow }
                    "* 1st-party cookie allow",
                    { Source = Source.Any
                      Destination = Destination.FirstParty
                      RequestType = RequestType.Cookie
                      Action = Action.Allow }
                    "* 1st-party frame allow",
                    { Source = Source.Any
                      Destination = Destination.FirstParty
                      RequestType = RequestType.Frame
                      Action = Action.Allow }
                    "* 1st-party other allow",
                    { Source = Source.Any
                      Destination = Destination.FirstParty
                      RequestType = RequestType.Other
                      Action = Action.Allow }
                    "* aadg.akadns.net fetch allow",
                    { Source = Source.Any
                      Destination = Destination.Specific "aadg.akadns.net"
                      RequestType = RequestType.Fetch
                      Action = Action.Allow }
                    "microsoft.com aadg.akadns.net frame allow",
                    { Source = Source.Specific "microsoft.com"
                      Destination = Destination.Specific "aadg.akadns.net"
                      RequestType = RequestType.Frame
                      Action = Action.Allow } ]

              testTheory "Parse valid rule" validRuleTheoryData (fun (ruleString, expected) ->
                  ruleString
                  |> Rule.create
                  |> Expect.wantOk "should parse to rule"
                  |> Expect.equal "should equal expected" expected)

              // theory data
              let invalidRuleTheoryData = [ "src.com 1st-party * allow" ]

              testTheory "Invalid rule" invalidRuleTheoryData (fun r ->
                  r |> Rule.create |> Expect.wantError "should return error")

              // theory data
              let sourceTheoryData =
                  [ "* * * block", "src.com * * allow"
                    "src.com * * block", "www.src.com * * allow" ]

              testTheory
                  "More qualified source hostname takes precedence over less qualified one"
                  sourceTheoryData
                  (fun (lessQualified, moreQualified) ->
                      result {
                          let! broader = lessQualified |> Rule.create
                          let! narrower = moreQualified |> Rule.create
                          narrower |> Rule.compare broader |> Expect.equal "" MoreSpecific
                      })

              // theory data
              let irrelevantSourceTheoryData =
                  [ "sub1.src.com * * block", "sub2.src.com * * allow" ]

              testTheory "Irrelevant source hostname" irrelevantSourceTheoryData (fun (r1, r2) ->
                  result {
                      let! rule1 = r1 |> Rule.create
                      let! rule2 = r2 |> Rule.create
                      rule2 |> Rule.compare rule1 |> Expect.equal "" Irrelevant
                  })

              // theory data
              let destinationTheoryData =
                  [ "* * * block", "* 1st-party * allow"
                    "* * * block", "* dst.com * allow"
                    "* 1st-party * block", "* dst.com * allow"
                    "* dst.com * block", "* www.dst.com * allow" ]

              testTheory
                  "More qualified destination hostname takes precedence over less qualified one"
                  destinationTheoryData
                  (fun (lessQualified, moreQualified) ->
                      result {
                          let! broader = lessQualified |> Rule.create
                          let! narrower = moreQualified |> Rule.create
                          narrower |> Rule.compare broader |> Expect.equal "" MoreSpecific
                      })

              // theory data
              let irrelevantDestinationTheoryData =
                  [ "* sub1.dst.com * block", "* sub2.dst.com * allow" ]

              testTheory "Irrelevant destination hostnames" irrelevantDestinationTheoryData (fun (d1, d2) ->
                  result {
                      let! destination1 = d1 |> Rule.create
                      let! destination2 = d2 |> Rule.create
                      destination2 |> Rule.compare destination1 |> Expect.equal "" Irrelevant
                  })

              // theory data
              let requestTypeTheoryData =
                  [ "* * * block", "* * cookie allow"
                    "* 1st-party * block", "* 1st-party css allow"
                    "* dst.com * block", "* dst.com image allow"
                    "src.com * * block", "src.com * script allow"
                    "src.com dst.com * block", "src.com dst.com fetch allow" ]

              testTheory
                  "More qualified request type takes precedence over less qualified one"
                  requestTypeTheoryData
                  (fun (lessQualified, moreQualified) ->
                      result {
                          let! broader = lessQualified |> Rule.create
                          let! narrower = moreQualified |> Rule.create
                          narrower |> Rule.compare broader |> Expect.equal "" MoreSpecific
                      })

              // theory data
              let irrelevantRequestTypeTheoryData = [ "* * script block", "* * fetch allow" ]

              testTheory "Irrelevant request type" irrelevantRequestTypeTheoryData (fun (t1, t2) ->
                  result {
                      let! requestType1 = t1 |> Rule.create
                      let! requestType2 = t2 |> Rule.create
                      requestType2 |> Rule.compare requestType1 |> Expect.equal "" Irrelevant
                  })

              // theory data
              let ruleTheoryData =
                  [ "* dst.com * block", "* dst.com script allow"
                    "* dst.com fetch block", "* www.dst.com fetch allow"
                    "src.com dst.com * block", "src.com dst.com script allow"
                    "src.com dst.com script block", "www.src.com dst.com script allow"
                    "src.com dst.com fetch block", "src.com www.dst.com fetch allow" ]

              testTheory "More fine-grained rule takes precedence" ruleTheoryData (fun (r1, r2) ->
                  result {
                      let! rule1 = r1 |> Rule.create
                      let! rule2 = r2 |> Rule.create
                      Rule.compare rule1 rule2 |> Expect.equal "" MoreSpecific
                  })

              // theory data
              let irrelevantRuleTheoryData =
                  [ "* 1st-party * allow", "src.com dst.com script allow"
                    "* 1st-party * allow", "www.src.com src.com script allow" // "src.com www.src.com script allow" would be more specific
                    "src.com dst.com script allow", "www.src.com * * allow" ]

              testTheory "Irrelevant rule" irrelevantRuleTheoryData (fun (r1, r2) ->
                  result {
                      let! rule1 = r1 |> Rule.create
                      let! rule2 = r2 |> Rule.create
                      Rule.compare rule1 rule2 |> Expect.equal "" Irrelevant
                      Rule.comparer rule1 rule2 |> Expect.equal "" -1
                  })

              // theory data
              let noMatchingRule =
                  [ "", "src.com dst.com script"
                    """* 1st-party * allow
                       * dst.com css allow
                       src.com dst.com image allow""",
                    "src.com dst.com script" ]

              testTheory "No matching rule" noMatchingRule (fun (rules, requestString) ->
                  result {
                      let ruleset = rules |> Ruleset.create
                      let! request = requestString |> Request.create
                      request |> Ruleset.evaluate ruleset |> Expect.equal "" None
                  })

              // theory data
              let hasMatchingRulesTheoryData =
                  [ """* * * block
                       * 1st-party * allow""",
                    "src.com src.com cookie",
                    "* 1st-party * allow"

                    """* * * block
                       * 1st-party * allow""",
                    "src.com www.src.com cookie",
                    "* 1st-party * allow"

                    """* * * block
                       src.com * * block
                       src.com dst.com css allow""",
                    "src.com dst.com css",
                    "src.com dst.com css allow"

                    """* * * block
                       src.com * * block
                       src.com dst.com css allow""",
                    "src.com www.dst.com css",
                    "src.com dst.com css allow"

                    """* * * block
                       www.src.com * * block
                       src.com dst.com css allow""",
                    "www.src.com www.dst.com css",
                    "www.src.com * * block"

                    """* * * block
                       src.com * * block
                       src.com dst.com css allow""",
                    "src.com dst.com image",
                    "src.com * * block" ]

              testTheory
                  "There are matching rule(s) => should take the most specific one"
                  hasMatchingRulesTheoryData
                  (fun (rules, requestString, expectedMatch) ->
                      result {
                          let ruleset = rules |> Ruleset.create
                          let! request = requestString |> Request.create
                          let mostSpecificMatch = request |> Ruleset.evaluate ruleset

                          mostSpecificMatch
                          |> Expect.wantSome "should have matching rule"
                          |> Rule.toString
                          |> Expect.equal "should take the most specific one" expectedMatch
                      })

              // theory data
              let hasMatchingRulesAndTheyAreIrrelevantTheoryData =
                  [ """* * * block
                       * dst.com css block
                       src.com * * allow""",
                    "src.com www.dst.com css",
                    "src.com * * allow"

                    """* * * block
                       src.com * * allow
                       * dst.com css block""",
                    "src.com www.dst.com css",
                    "src.com * * allow" ]

              testTheory
                  "There are matching rules, but they are irrelevant => should take the highest precedence"
                  hasMatchingRulesAndTheyAreIrrelevantTheoryData
                  (fun (rules, requestString, expectedMatch) ->
                      result {
                          let ruleset = rules |> Ruleset.create
                          let! request = requestString |> Request.create
                          let mostSpecificMatch = request |> Ruleset.evaluate ruleset

                          mostSpecificMatch
                          |> Expect.wantSome "should have matching rule"
                          |> Rule.toString
                          |> Expect.equal "should take the most specific one" expectedMatch
                      }) ]
