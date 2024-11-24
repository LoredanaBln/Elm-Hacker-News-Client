module View.Posts exposing (..)

import Html exposing (Html, a, div, label, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href)
import Html.Events
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable config currentTime posts =
    let
        filteredPosts =
            filterPosts config posts
    in
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Score" ]
                , th [] [ text "Title" ]
                , th [] [ text "Type" ]
                , th [] [ text "Posted Date" ]
                , th [] [ text "Link" ]
                ]
            ]
        , tbody []
            (List.map (postRow currentTime) filteredPosts)
        ]


postRow : Time.Posix -> Post -> Html Msg
postRow currentTime post =
    let
        duration =
            Util.Time.durationBetween post.time currentTime |> Maybe.withDefault { seconds = 0, minutes = 0, hours = 0, days = 0 }
    in
    tr []
        [ td [ class "post-score" ] [ text (String.fromInt post.score) ]
        , td [ class "post-title" ] [ text post.title ]
        , td [ class "post-type" ] [ text post.type_ ]
        , td [ class "post-time" ] [ text (Util.Time.formatTime Time.utc post.time ++ " (" ++ Util.Time.formatDuration duration ++ ")") ]
        , td [ class "post-url" ]
            [ case post.url of
                Just url ->
                    text url

                Nothing ->
                    text "No URL"
            ]
        ]


{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    div []
        [ div []
            [ label [ Html.Attributes.for "select-posts-per-page" ] [ text "Posts per page:" ]
            , Html.select
                [ Html.Attributes.id "select-posts-per-page"
                , Html.Events.onInput (\value -> ConfigChanged (ChangePostsToShow (String.toInt value |> Maybe.withDefault 10)))
                ]
                [ Html.option [ Html.Attributes.value "10", Html.Attributes.selected (config.postsToShow == 10) ] [ text "10" ]
                , Html.option [ Html.Attributes.value "25", Html.Attributes.selected (config.postsToShow == 25) ] [ text "25" ]
                , Html.option [ Html.Attributes.value "50", Html.Attributes.selected (config.postsToShow == 50) ] [ text "50" ]
                ]
            ]
        , div []
            [ label [ Html.Attributes.for "select-sort-by" ] [ text "Sort by:" ]
            , Html.select
                [ Html.Attributes.id "select-sort-by"
                , Html.Events.onInput (sortFromString >> Maybe.map (ChangeSortBy >> ConfigChanged) >> Maybe.withDefault (ConfigChanged (ChangeSortBy None)))
                ]
                (List.map
                    (\sortOption ->
                        Html.option
                            [ Html.Attributes.value (sortToString sortOption)
                            , Html.Attributes.selected (config.sortBy == sortOption)
                            ]
                            [ text (sortToString sortOption) ]
                    )
                    sortOptions
                )
            ]
        , div []
            [ label [ Html.Attributes.for "checkbox-show-job-posts" ] [ text "Show job posts:" ]
            , Html.input
                [ Html.Attributes.id "checkbox-show-job-posts"
                , Html.Attributes.type_ "checkbox"
                , Html.Attributes.checked config.showJobs
                , Html.Events.onCheck (ConfigChanged << ChangeShowJobs)
                ]
                []
            ]
        , div []
            [ label [ Html.Attributes.for "checkbox-show-text-only-posts" ] [ text "Show text-only posts:" ]
            , Html.input
                [ Html.Attributes.id "checkbox-show-text-only-posts"
                , Html.Attributes.type_ "checkbox"
                , Html.Attributes.checked config.showTextOnly
                , Html.Events.onCheck (ConfigChanged << ChangeShowTextOnly)
                ]
                []
            ]
        ]
