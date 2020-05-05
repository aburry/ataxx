module Matrix exposing
    ( Matrix
    , get
    , initialize
    , set
    )

import Array exposing (..)


type alias Matrix a =
    { data : Array a
    , rows : Int
    , cols : Int
    }


initialize : Int -> Int -> (Int -> Int -> a) -> Matrix a
initialize row col fn =
    { rows = row
    , cols = col
    , data = Array.initialize (row * col) (\e -> fn (e // row) (remainderBy row e))
    }


isValid row col m =
    0 <= row && row < m.rows && 0 <= col && col < m.cols


get : Int -> Int -> Matrix a -> Maybe a
get row col m =
    if isValid row col m then
        Array.get (row * m.rows + col) m.data

    else
        Maybe.Nothing


set : Int -> Int -> a -> Matrix a -> Matrix a
set row col value m =
    if isValid row col m then
        { m | data = Array.set (row * m.rows + col) value m.data }

    else
        m
