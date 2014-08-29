# requires SQL Server 2012 LocalDb
SqlLocalDb create UnoNes -s
sqlcmd -S "(localdb)\Uno" -E -Q "CREATE DATABASE UnoNes"