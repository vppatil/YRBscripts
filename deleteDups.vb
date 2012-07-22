

Sub DeleteDuplicateRecords2(strTableName As String)
    ' Deletes exact duplicates from the specified table.
    ' No user confirmation is required. Use with caution.
    Dim rst As DAO.Recordset
    Dim rst2 As DAO.Recordset
    Dim tdf As DAO.TableDef
    Dim fld As DAO.Field
    Dim strSQL As String
    Dim varBookmark As Variant

    Set tdf = DBEngine(0)(0).TableDefs(strTableName)
    strSQL = "SELECT * FROM " & strTableName & " ORDER BY "
    ' Build a sort string to make sure duplicate records are
    ' adjacent. Can't sort on OLE or Memo fields,though.
    For Each fld In tdf.Fields
        If (fld.Type <> adLongVarWChar) And (fld.Type <> adLongVarBinary) Then
            strSQL = strSQL & fld.Name & ", "
        End If
    Next
    
    ' Remove the extra comma and space from the SQL
    strSQL = Left(strSQL, Len(strSQL) - 2)
    Set tdf = Nothing

    Set rst = CurrentDb.OpenRecordset(strSQL)
    Set rst2 = rst.Clone
    rst.MoveNext
    Do Until rst.EOF
        varBookmark = rst.Bookmark
        For i = 0 To 13
            If rst.Fields(i).Value <> rst2.Fields(i).Value Then
                GoTo NextRecord
            End If
        Next
        rst.Delete
        GoTo SkipBookmark
NextRecord:
        rst2.Bookmark = varBookmark
SkipBookmark:
        rst.MoveNext
    Loop
End Sub



