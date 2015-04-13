{-

  The haskell base for the Agda implementation of a Relational Algebra
    -- Toon Nolten

-}
module RelationalBase (connectSqlite3, read) where

import Database.HDBC.sqlite3 (connectSqlite3)
import Database.HDBC

type DatabasePath = String
type TableName = String

-- Read : all s -> Handle s -> RA s
read :: IConnection conn => conn -> TableName
            -> IO [[SqlValue]]
read conn table =
    quickQuery' conn
        "SELECT * FROM "++ table
