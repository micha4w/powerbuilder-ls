forward
global type zes1_u_zes_filehandler from nonvisualobject
end type
end forward

global type zes1_u_zes_filehandler from nonvisualobject
end type
global zes1_u_zes_filehandler zes1_u_zes_filehandler

type prototypes
// https://docs.microsoft.com/en-us/windows/win32/winprog/windows-data-types

//Öffnen/Erstellt eine Datei
//	https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilew
function ulong lef_createfile ( &
		string lpfilename, &
		ulong dwdesiredaccess, &
		ulong dwsharemode, &
		ulong lpsecurityattributes, &
		ulong dwcreationdisposition, &
		ulong dwflagsandattributes, &
		ulong htemplatefile &
	) library 'kernel32.dll' alias for 'CreateFileW'

//Schliesst eine Datei
//	https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-findclose
function boolean lef_closehandle ( &
		ulong hobject &
	) library 'kernel32.dll' alias for 'CloseHandle'

//Markiert aktuelle Cursorpostion in Datei als Dateiende (löscht Inhalt der Datei ab Cursorposition)
//	https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-setendoffile
function boolean lef_setendoffile( &
		ulong hfile &
	) library 'kernel32.dll' alias for 'SetEndOfFile'

//List aus einer Datei
//	https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile
function boolean lef_readfile ( &
		ulong hfile, &
		ref blob lpbuffer, &
		ulong nnumberofbytestoread, &
		ref ulong lpnumberofbytesread, &
		ulong lpoverlapped &
	) library 'kernel32.dll' alias for 'ReadFile'

//Schreibt in eine Datei
//	https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-writefile
function boolean lef_writefile ( &
		ulong hfile, &
		blob lpbuffer, &
		ulong nnumberofbytestowrite, &
		ref ulong lpnumberofbyteswritten, &
		ulong lpoverlapped &
	) library 'kernel32.dll' alias for 'WriteFile'

//Gibt den letzten ErrorCode zurück, der aufgetreten ist
//	https://docs.microsoft.com/de-de/windows/win32/api/errhandlingapi/nf-errhandlingapi-getlasterror
function ulong lef_getlasterror ( &
	) library 'kernel32.dll' alias for 'GetLastError'

//Gibt die Grösse einer Datei (Anzahl Bytes) zurück
// https://docs.microsoft.com/de-de/windows/win32/api/fileapi/nf-fileapi-getfilesize
function ulong lef_getfilesize ( &
		ulong hfile, &
		ulong lpfilesizehigh &
	) library 'kernel32.dll' alias for 'GetFileSize'

//Setzt den Cursor in einer Datei an eine neue Position
// https://docs.microsoft.com/de-de/windows/win32/api/fileapi/nf-fileapi-getfilesize
function boolean lef_setfilepointer ( &
		ulong hfile, &
		longlong ldistancetomove, &
		ref longlong lpnewfilepointer, &
		ulong dwmovemethod &
	) library 'kernel32.dll' alias for 'SetFilePointerEx'

end prototypes

type variables
ulong iul_filehandle[] //Alle offenen Handles der Dateien, die mit Win32 API geöffnet wurden


//Konstanten für Win32 API
constant ulong CUL_INVALID_HANDLE_VALUE	= -1 //-1 wird zu 2^32 - 1, (unsigned long) das spielt aber keine Rolle
constant ulong CUL_GENERIC_READ				= 2147483648 //0x80000000
constant ulong CUL_GENERIC_WRITE			= 1073741824 //0x40000000
constant ulong CUL_FILE_SHARE_READ			= 1
constant ulong CUL_FILE_SHARE_WRITE		= 2
constant ulong CUL_CREATE_NEW				= 1
constant ulong CUL_CREATE_ALWAYS			= 2
constant ulong CUL_OPEN_EXISTING			= 3
constant ulong CUL_OPEN_ALWAYS				= 4
constant ulong CUL_TRUNCATE_EXISTING		= 5
constant ulong CUL_FILE_BEGIN				= 0
constant ulong CUL_FILE_CURRENT				= 1
constant ulong CUL_FILE_END					= 2
end variables

forward prototypes
public function integer of_open (string as_filename) throws u_zes_exception_fileio
public function boolean of_save_to (integer ai_filehandle, string as_file)
public function boolean of_close (integer ai_filehandle)
public function boolean of_empty (integer ai_filehandle)
end prototypes

public function integer of_open (string as_filename) throws u_zes_exception_fileio;//Zweck		Öffnet Datei im lockwrite-Mode
//				Wird benötigt, weil PB unterstützt nicht das gleichzeitige Lesen und schreiben in eine Datei
//Argument	as_filename Absoluterpfad zur Datei
//Throws		u_zes_exception_fileio
//Return		integer PB-Handle zur Datei im LineMode und mit Read-Zugriff ACHTUNG: File muss zwingend mit of_close geschlossen werden!
//Erstellt	2019-11-01 Simon Reichenbach
//Geändert	2019-11-21 Simon Reichenbach	Win32 API öffnet Datei neuerdings im READ/WRITE- statt nur im WRITE-Mode

u_zes_exception_fileio le_e
long lul_filehandle //Win32 FileHandle
integer li_filehandle //PB FileHandle

//Datei öffnen
if fileexists(as_filename) then
	
	//Datei im Schreibmodus mittels Win32 API öffnen
	lul_filehandle = lef_createfile(as_filename, CUL_GENERIC_WRITE+CUL_GENERIC_READ, &
												CUL_FILE_SHARE_READ, 0, CUL_OPEN_EXISTING, 0, 0)
	if lul_filehandle = CUL_INVALID_HANDLE_VALUE then
		le_e = create u_zes_exception_fileio
		le_e.setmessage('Datei ' + as_filename + ' konnte nicht geöffnet werden (Win32 API)')
		throw(le_e)
	end if
	
	//Datei im Lesemodus mittels PB API öffnen
	li_filehandle =  gf_long_replace_null(fileopen(as_filename, linemode!, read!, shared!))
	if not li_filehandle > 0 then
		le_e = create u_zes_exception_fileio
		le_e.setmessage('Datei ' + as_filename + ' konnte nicht geöffnet werden (PB API)')
		lef_closehandle(lul_filehandle)
		throw(le_e)
	end if
	
	//Filehandle der Win32 API speichern, um Datei in of_close() wieder schliessen zu können
	iul_filehandle[li_filehandle] = lul_filehandle
	return li_filehandle
else
	le_e = create u_zes_exception_filenotfound
	le_e.setmessage('Datei ' + gf_string_replace_null(as_filename) + ' existiert nicht')
	throw(le_e)
end if

return -1 
end function

public function boolean of_save_to (integer ai_filehandle, string as_file);//Zweck		Speichert den gesamten Inhalt einer Datei in eine andere (Sicherungs-)Datei (Append)
//Argument	ai_filehandle	Handle zur Datei, die gesichert werden soll
//				as_file	Datei, in welche die Daten gespeichert werden sollen
//Return		true	Erfolgreich
//				false	Fehler
//Erstellt	2019-11-21 Simon Reichenbach

blob lb_data
ulong lul_filesize
ulong lul_fileread
integer li_backfile

if iul_filehandle[ai_filehandle] > 0 then
	lul_filesize = lef_getfilesize(iul_filehandle[ai_filehandle], 0)
	lb_data = blob(fill(' ', lul_filesize))
	if lef_readfile(iul_filehandle[ai_filehandle], lb_data, lul_filesize, lul_fileread, 0) then
		if lul_filesize = lul_fileread then
			li_backfile = fileopen(as_file, streammode!, write!, shared!, append!)
			try
				if filewriteex(li_backfile, lb_data, lul_filesize) >= 0 then
					return true
				end if
			finally
				fileclose(li_backfile)
			end try
		end if
	end if
end if

return false
end function

public function boolean of_close (integer ai_filehandle);//Zweck		Schliesst die beiden Filehandles (Win32 & PB), welche von zes1_u_zes_file verwaltet wird
//Argument	FileHandle (welcher von u_zes_filehandle.of_open() stammt
//Return		true	Datei wurde erfolgreich geschlossen
//				false	sonst
//Erstellt	2019-11-01 Simon Reichenbach

long ll_ret 
ulong lul_filehandle
boolean lbo_ret

ll_ret = fileclose(ai_filehandle)

if iul_filehandle[ai_filehandle] > 0 then
	lul_filehandle = iul_filehandle[ai_filehandle]
	lbo_ret = lef_closehandle(lul_filehandle)
	if lbo_ret then
		iul_filehandle[ai_filehandle] = 0
		return ll_ret = 1
	else
		runtimeerror lr_e
		lr_e = create runtimeerror
		lr_e.setmessage('Fehler beim Schliessen eines File-Handles.~r~nWin32-ErrorCode: ' + string(gf_long_replace_null(lef_getlasterror())))
		throw(lr_e)
	end if
else
	return ll_ret = 1
end if

return false
end function

public function boolean of_empty (integer ai_filehandle);//Zweck		Löscht den Inhalt einer Datei ab dem aktuellen FilePointer des Win32 API-FileHandles
//				Wird benötigt, umn die Datei zu leeren, sobals sie fertig importiert wurde
//Argument	li_filehandle	PB-FileHandle, welcher durch of_open zurückgegeben wurde
//Return		true	Datei wurde erfolgreich geleert
//				false	sonst
//Erstellt	2019-11-01 Simon Reichenbach

if iul_filehandle[ai_filehandle] > 0 then
	longlong ll_newptr
	if lef_setfilepointer(iul_filehandle[ai_filehandle], 0, ll_newptr, CUL_FILE_BEGIN) then
		return lef_setendoffile(iul_filehandle[ai_filehandle])
	end if
end if

return false

end function

on zes1_u_zes_filehandler.create
call super::create
TriggerEvent( this, "constructor" )
end on

on zes1_u_zes_filehandler.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

