{- | A file server for frequently accessed files, such are static web pages and image decorations, icons etc
that are cached (memoized) according with the "Data.TCache" policies in the program space. This avoid the blocking of
the efficient GHC threads by frequent IO calls.So it enhances the performance
in the context of heavy concurrence.
It uses 'Data.TCache.Memoization'.
The caching-uncaching follows the `setPersist` criteria.
-}
-----------------------------------------------------------------------------
--
-- Module      :  FileServer
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
--
--
-----------------------------------------------------------------------------
{-# OPTIONS -XScopedTypeVariables  #-}
module MFlow.FileServer (addFileServerWF, linkFile,setFilesPath

) where

import MFlow
import Control.Monad.State
import Data.TCache.Memoization
import MFlow.Forms.XHtml
import System.Directory
import Data.ByteString.Lazy.Char8 as B(readFile,concat,append,pack,empty)

import Control.Exception as CE
import Data.Char
import Data.List
import System.IO.Unsafe
import Data.IORef
import Data.Monoid


rfilesPath= unsafePerformIO $ newIORef "files/"

-- | Set the path of the files in the web server. The links to the files are relative to it
setFilesPath :: String -> IO ()
setFilesPath path= writeIORef rfilesPath path

pathPrm=  "path"
fileServe ::(Token -> Workflow IO ())
fileServe  = stateless $ \env  -> do
  case lookup pathPrm   env of
    Nothing -> error " no file specified"
    Just path' ->do
     when(let hpath= head path' in hpath == '/' || hpath =='\\') $ error noperm
     when(not(".." `isSuffixOf` path') && ".." `isInfixOf` path') $ error noperm
     filesPath <- readIORef rfilesPath
     let path= filesPath ++ path'
     servefile path
    -- isDirectory <- doesDirectoryExist  path -- !> path

--     case isDirectory of
--       True  -> do
--          dir <-  directory1 $ path ++ "/"
--          return $ HttpData  (setMime "text/html") []  dir
--       False -> servefile path


 where
 setMime x= [("Content-Type",x)]

 dropBack ".."= ".."
 dropBack path
     | "../" `isPrefixOf` revpath =reverse . maybetail $ dropWhile (/= '/') $ drop 3 revpath
     | otherwise= path
   where
   revpath= reverse path
--   maybetail ""= "."
   maybetail xs= tail xs
 noperm= "no permissions"
 ioerr x= \(e :: CE.IOException) ->  x
 servefile path= do
     mr <-  cachedByKey path 0  $   (B.readFile  path >>=  return . Just) `CE.catch` ioerr (return Nothing)
     case mr of
      Nothing -> return $ HttpData  (setMime "text/plain") [] $ pack $  "not accessible"
      Just r ->
         let ext  = reverse . takeWhile (/='.') $ reverse path
             mmime= lookup (map toLower ext) mimeTable
             mime = case mmime of Just m -> m ;Nothing -> "application/octet-stream"

         in return $ HttpData  (setMime mime) [] r

-- | Is the flow to be added to the list in order to stream any file from the filesystem
-- for example, images
--
-- This app includes the fileServe  flow:
--
-- @
-- main= do
--   addFileServerWF
--   addMessageFlows messageFlows
--   run 80  hackMessageFlow
--   adminLoop
--   where
--   messageFlows=  [(\"noscript\" , transient $ runFlow showStories)
--                  ,("\admin\"    , transient $ runFlow admin)
--                  ,("\mail\"     , transient $ runFlow mail)]@
--
-- | Add the fileServer to the list of server flows
-- it is mandatory if you want to use the file service
addFileServerWF= addMessageFlows [("file", fileServe)]



-- | Creates the url of file path. To be used in ordinary links to files.
-- in Text.XHtml, a image would be embeded as
--
-- > image ![src $ linkFile imagepath]
--
-- in HSP:
--
-- > <img src=(linkFile imagepath)\>

-- | Given the relative path of a file, it return the content of the @href@ element in a html link
linkFile :: String -> String
linkFile path=  "file?path=" <>  path

directory :: Token -> Workflow IO ()
directory = stateless $ \_ -> do
   path <- readIORef rfilesPath
   directory1 path

directory1 path = do
   fs <- getDirectoryContents path
   return $ B.concat [btag "a" [("href",linkFile ( path ++  file))] (B.pack file) `append` btag "br" [] B.empty | file <- fs]


mimeTable=[
    ("html",	"text/html"),
    ("htm",	"text/html"),
    ("txt",	"text/plain"),
    ("jpeg",	"image/jpeg"),
    ("pdf",	"application/pdf"),
    ("js",	"application/x-javascript"),
    ("gif",	"image/gif"),
    ("bmp",	"image/bmp"),
    ("ico",	"image/x-icon"),
    ("doc",	"application/msword"),
    ("jpg",	"image/jpeg"),
    ("eps",	"application/postscript"),
    ("zip",	"application/zip"),
    ("exe",	"application/octet-stream"),
    ("tif",	"image/tiff"),
    ("tiff",	"image/tiff"),
    ("mov",	"video/quicktime"),
    ("movie",	"video/x-sgi-movie"),
    ("mp2",	"video/mpeg"),
    ("mp3",	"audio/mpeg"),
    ("mpa",	"video/mpeg"),
    ("mpe",	"video/mpeg"),
    ("mpeg",	"video/mpeg"),
    ("mpg",	"video/mpeg"),
    ("mpp",	"application/vnd.ms-project"),
    ("323",	"text/h323"),
    ("*",	"application/octet-stream"),
    ("acx",	"application/internet-property-stream"),
    ("ai",	"application/postscript"),
    ("aif",	"audio/x-aiff"),
    ("aifc",	"audio/x-aiff"),
    ("aiff",	"audio/x-aiff"),
    ("asf",	"video/x-ms-asf"),
    ("asr",	"video/x-ms-asf"),
    ("asx",	"video/x-ms-asf"),
    ("au",	"audio/basic"),
    ("avi",	"video/x-msvideo"),
    ("axs",	"application/olescript"),
    ("bas",	"text/plain"),
    ("bcpio",	"application/x-bcpio"),
    ("bin",	"application/octet-stream"),
    ("c",	"text/plain"),
    ("cat",	"application/vnd.ms-pkiseccat"),
    ("cdf",	"application/x-cdf"),
    ("cdf",	"application/x-netcdf"),
    ("cer",	"application/x-x509-ca-cert"),
    ("class",	"application/octet-stream"),
    ("clp",	"application/x-msclip"),
    ("cmx",	"image/x-cmx"),
    ("cod",	"image/cis-cod"),
    ("cpio",	"application/x-cpio"),
    ("crd",	"application/x-mscardfile"),
    ("crl",	"application/pkix-crl"),
    ("crt",	"application/x-x509-ca-cert"),
    ("csh",	"application/x-csh"),
    ("css",	"text/css"),
    ("dcr",	"application/x-director"),
    ("der",	"application/x-x509-ca-cert"),
    ("dir",	"application/x-director"),
    ("dll",	"application/x-msdownload"),
    ("dms",	"application/octet-stream"),
    ("dot",	"application/msword"),
    ("dvi",	"application/x-dvi"),
    ("dxr",	"application/x-director"),
    ("eps",	"application/postscript"),
    ("etx",	"text/x-setext"),
    ("evy",	"application/envoy"),
    ("fif",	"application/fractals"),
    ("flr",	"x-world/x-vrml"),
    ("gtar",	"application/x-gtar"),
    ("gz",	"application/x-gzip"),
    ("h",	"text/plain"),
    ("hdf",	"application/x-hdf"),
    ("hlp",	"application/winhlp"),
    ("hqx",	"application/mac-binhex40"),
    ("hta",	"application/hta"),
    ("htc",	"text/x-component"),
    ("htt",	"text/webviewhtml"),
    ("ief",	"image/ief"),
    ("iii",	"application/x-iphone"),
    ("ins",	"application/x-internet-signup"),
    ("isp",	"application/x-internet-signup"),
    ("jfif",	"image/pipeg"),
    ("jpe",	"image/jpeg"),
    ("latex",	"application/x-latex"),
    ("lha",	"application/octet-stream"),
    ("lsf",	"video/x-la-asf"),
    ("lsx",	"video/x-la-asf"),
    ("lzh",	"application/octet-stream"),
    ("m13",	"application/x-msmediaview"),
    ("m14",	"application/x-msmediaview"),
    ("m3u",	"audio/x-mpegurl"),
    ("man",	"application/x-troff-man"),
    ("mdb",	"application/x-msaccess"),
    ("me",	"application/x-troff-me"),
    ("mht",	"message/rfc822"),
    ("mhtml",	"message/rfc822"),
    ("mid",	"audio/mid"),
    ("mny",	"application/x-msmoney"),
    ("mpv2",	"video/mpeg"),
    ("ms",	"application/x-troff-ms"),
    ("msg",	"application/vnd.ms-outlook"),
    ("mvb",	"application/x-msmediaview"),
    ("nc",	"application/x-netcdf"),
    ("nws",	"message/rfc822"),
    ("oda",	"application/oda"),
    ("p10",	"application/pkcs10"),
    ("p12",	"application/x-pkcs12"),
    ("p7b",	"application/x-pkcs7-certificates"),
    ("p7c",	"application/x-pkcs7-mime"),
    ("p7m",	"application/x-pkcs7-mime"),
    ("p7r",	"application/x-pkcs7-certreqresp"),
    ("p7s",	"application/x-pkcs7-signature"),
    ("pbm",	"image/x-portable-bitmap"),
    ("pfx",	"application/x-pkcs12"),
    ("pgm",	"image/x-portable-graymap"),
    ("pko",	"application/ynd.ms-pkipko"),
    ("pma",	"application/x-perfmon"),
    ("pmc",	"application/x-perfmon"),
    ("pml",	"application/x-perfmon"),
    ("pmr",	"application/x-perfmon"),
    ("pmw",	"application/x-perfmon"),
    ("pnm",	"image/x-portable-anymap"),
    ("pot",	"application/vnd.ms-powerpoint"),
    ("ppm",	"image/x-portable-pixmap"),
    ("pps",	"application/vnd.ms-powerpoint"),
    ("ppt",	"application/vnd.ms-powerpoint"),
    ("prf",	"application/pics-rules"),
    ("ps",	"application/postscript"),
    ("pub",	"application/x-mspublisher"),
    ("qt",	"video/quicktime"),
    ("ra",	"audio/x-pn-realaudio"),
    ("ram",	"audio/x-pn-realaudio"),
    ("ras",	"image/x-cmu-raster"),
    ("rgb",	"image/x-rgb"),
    ("rmi",	"audio/mid"),
    ("roff",	"application/x-troff"),
    ("rtf",	"application/rtf"),
    ("rtx",	"text/richtext"),
    ("scd",	"application/x-msschedule"),
    ("sct",	"text/scriptlet"),
    ("setpay",	"application/set-payment-initiation"),
    ("setreg",	"application/set-registration-initiation"),
    ("sh",	"application/x-sh"),
    ("shar",	"application/x-shar"),
    ("sit",	"application/x-stuffit"),
    ("snd",	"audio/basic"),
    ("spc",	"application/x-pkcs7-certificates"),
    ("spl",	"application/futuresplash"),
    ("src",	"application/x-wais-source"),
    ("sst",	"application/vnd.ms-pkicertstore"),
    ("stl",	"application/vnd.ms-pkistl"),
    ("stm",	"text/html"),
    ("sv4cpio",	"application/x-sv4cpio"),
    ("sv4crc",	"application/x-sv4crc"),
    ("svg",	"image/svg+xml"),
    ("swf",	"application/x-shockwave-flash"),
    ("t",	"application/x-troff"),
    ("tar",	"application/x-tar"),
    ("tcl",	"application/x-tcl"),
    ("tex",	"application/x-tex"),
    ("texi",	"application/x-texinfo"),
    ("texinfo",	"application/x-texinfo"),
    ("tgz",	"application/x-compressed"),
    ("tr",	"application/x-troff"),
    ("trm",	"application/x-msterminal"),
    ("tsv",	"text/tab-separated-values"),
    ("uls",	"text/iuls"),
    ("ustar",	"application/x-ustar"),
    ("vcf",	"text/x-vcard"),
    ("vrml",	"x-world/x-vrml"),
    ("wav",	"audio/x-wav"),
    ("wcm",	"application/vnd.ms-works"),
    ("wdb",	"application/vnd.ms-works"),
    ("wks",	"application/vnd.ms-works"),
    ("wmf",	"application/x-msmetafile"),
    ("wps",	"application/vnd.ms-works"),
    ("wri",	"application/x-mswrite"),
    ("wrl",	"x-world/x-vrml"),
    ("wrz",	"x-world/x-vrml"),
    ("xaf",	"x-world/x-vrml"),
    ("xbm",	"image/x-xbitmap"),
    ("xla",	"application/vnd.ms-excel"),
    ("xlc",	"application/vnd.ms-excel"),
    ("xlm",	"application/vnd.ms-excel"),
    ("xls",	"application/vnd.ms-excel"),
    ("xlt",	"application/vnd.ms-excel"),
    ("xlw",	"application/vnd.ms-excel"),
    ("xof",	"x-world/x-vrml"),
    ("xpm",	"image/x-xpixmap"),
    ("xwd",	"image/x-xwindowdump"),
    ("z",	"application/x-compress")

 ]

