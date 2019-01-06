module Helper exposing (emptyToNothing, isNothing, unwrap)


emptyToNothing : String -> Maybe String
emptyToNothing src =
    if String.isEmpty src then
        Nothing

    else
        Just src


isNothing : Maybe a -> Bool
isNothing src =
    case src of
        Just _ ->
            True

        Nothing ->
            False


unwrap : Maybe String -> String
unwrap src =
    Maybe.withDefault "" src
