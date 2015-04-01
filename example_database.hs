{-

  Set up an example Sqlite database and execute a couple of example queries
	-- Toon Nolten

-}

import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import Data.List


create_TopGear_table :: IConnection conn => conn -> IO Integer
create_TopGear_table conn =
    run conn
	"CREATE TABLE lap_times (Model CHAR(20), Time CHAR(6), Wet BOOL)"
	[]

prepare_insert_rows :: IConnection conn => conn -> IO Statement
prepare_insert_rows conn =
    prepare conn "INSERT INTO lap_times VALUES (?, ?, ?)"

initial_rows :: [[SqlValue]]
initial_rows = zipWith3 (\model time wet ->
			  [toSql model, toSql time, toSql wet])
		   ["Ascari A10", "Koenigsegg CCX",
		    "Pagani Zonda C12 F", "Maserati MC12"]
		   ["1:17.3", "1:17.6", "1:18.4", "1:18.9"]
		   [False, True, False, False]

main = handleSqlError $ do
	conn <- connectSqlite3 "TopGear.sqlite"
	create_TopGear_table conn
	insert_stmt <- prepare_insert_rows conn
	executeMany insert_stmt initial_rows
	r <- quickQuery' conn "SELECT Model, Time, Wet from lap_times ORDER BY Time" []
	let stringRows = map convRow r
	mapM_ putStrLn stringRows
	commit conn
	disconnect conn
	where convRow :: [SqlValue] -> String
	      convRow [sqlModel, sqlTime, sqlWet] =
		show model ++ " -- " ++ time ++ " -- " ++ wet
		where model = case fromSql sqlModel of
				Just x -> x
				Nothing -> "NULL"
		      time = case fromSql sqlTime of
				Just x -> x
				Nothing -> "NULL"
		      wet = fromSql sqlWet
	      convRow x = fail $ "Unexpected result: " ++ show x
