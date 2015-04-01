{-

  The haskell base for the Agda implementation of a Relational Algebra
    -- Toon Nolten

-}

import Database.HDBC.sqlite3 (connectSqlite3)
import Database.HDBC

type TableName = String
type Constraint = String

comma_separated = concat . (intersperse ", ")

-- Read : all s -> Handle s -> RA s
read :: IConnection conn => conn -> TableName
            -> IO [[SqlValue]]
read conn table =
    quickQuery' conn
        SELECT * FROM "++ table

-- Union : all s -> RA s -> RA s -> RA s
union :: IConnection conn => conn -> TableName -> TableName
            -> IO [[SqlValue]]
union conn table table' =
    quickQuery' conn
        "SELECT * FROM "++ table ++" UNION SELECT * FROM "++ table'

-- Diff : all s -> RA s -> RA s -> RA s
diff :: IConnection conn => conn -> TableName -> TableName 
            -> IO [[SqlValue]]
diff conn table table' =
    quickQuery' conn
        "SELECT * FROM "++ table ++" EXCEPT SELECT * FROM "++ table'

-- Product : all s s' -> {So disjoint s s'} -> RA s -> RA s' -> RA (append s s')
product :: IConnection conn => conn -> TableName -> TableName
            -> IO [[SqlValue]]
product conn table table' =
    quickQuery' conn
        "SELECT * FROM "++ table ++" CROSS JOIN "++ table'

-- Project : all s -> s' : Schema -> {So sub s' s} -> RA s -> RA s'
project :: IConnection conn => conn -> TableName -> [ColumnName]
            -> IO [[SqlValue]]
project conn table columns =
    quickQuery' conn
        "SELECT "++ (comma_separated columns) ++" FROM "++ table

-- Select : all s -> Expr s BOOL -> RA s -> RA s
-- This assumes that a Constraint is a string containing a valid SQL constraint
select :: IConnection conn => conn -> TableName -> [Constraint]
            -> IO [[SqlValue]]
select conn table constraints =
    quickQuery' conn
        "SELECT * FROM "++ table ++" WHERE "++ (comma_separated constraints)


{-
  This implementation of the base for a relational algebra does not allow to
  nest expressions. You cannot *project* on the result of *select* because
  *select* returns a list of rows while *project* operates on tables (tables in
  a database to be precise.
-}
