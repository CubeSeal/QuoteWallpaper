{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module ImageMagick
  (  addQuoteToImage
  ) where

-- Modules
import Foreign.C.String   (CString, withCString)
import Foreign.C.Types    (CUInt(..))
import Foreign.Ptr        (Ptr, nullPtr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)

import Control.Monad.Except (MonadError, MonadIO, liftIO, liftEither)

import qualified Clippings as C
import qualified Data.Text.Lazy as T

-- Left Error Type
data ImageMagickError =
  NullPtrError
  | ReadFileFailure
  | OtherError String
  deriving (Show, Eq)

-- Opaque types
data CMagickWand
data CDrawingWand

-- MagickBooleanType is often an enum (0 for false, non-zero for true)
-- We'll map it to CInt and check for non-zero for success.
type MagickBooleanType = CUInt -- Or CInt, check MagickCore/magick-type.h

-- --- FFI Imports for MagickWand API ---

-- void MagickWandGenesis(void);
foreign import ccall safe "MagickWandGenesis"
  c_magickWandGenesis :: IO ()

-- void MagickWandTerminus(void);
foreign import ccall safe "MagickWandTerminus"
  c_magickWandTerminus :: IO ()

-- MagickWand *NewMagickWand(void);
foreign import ccall safe "NewMagickWand"
  c_newMagickWand :: IO (Ptr CMagickWand)

-- MagickBooleanType MagickReadImage(MagickWand *wand, const char *filename);
foreign import ccall safe "MagickReadImage"
  c_magickReadImage :: Ptr CMagickWand -> CString -> IO MagickBooleanType

-- DrawingWand *NewDrawingWand(void);
foreign import ccall safe "NewDrawingWand"
  c_newDrawingWand :: IO (Ptr CDrawingWand)

-- Image Magick Methods:
foreign import ccall safe "DrawSetFont"
  c_drawSetFont :: Ptr CDrawingWand -> CString -> IO ()

foreign import ccall safe "DrawSetFontSize"
  c_drawSetFontSize :: Ptr CDrawingWand -> CString -> IO ()

foreign import ccall safe "DrawAnnotation"
  c_drawSetAnnotation :: Ptr CDrawingWand -> CString -> IO ()

foreign import ccall safe "MagickDrawImage"
  c_magickDrawImage :: Ptr CMagickWand -> Ptr CDrawingWand -> IO ()

foreign import ccall safe "MagickWriteImage"
  c_magickWriteImage :: Ptr CMagickWand -> CString -> IO()

foreign import ccall safe "printf"
  c_printf :: CString -> IO ()

-- MagickWand *DestroyMagickWand(MagickWand *wand);
-- The API returns the destroyed wand, often NULL. We only care about the side effect.
-- We need a FunPtr for the finalizer.
foreign import ccall safe "&DestroyMagickWand"
  c_destroyMagickWand_funptr :: FunPtr (Ptr CMagickWand -> IO ())

foreign import ccall safe "&DestroyDrawingWand"
  c_destroyDrawingWand_funptr :: FunPtr (Ptr CDrawingWand -> IO ())

-- --- Haskell Wrappers ---

-- Helper to convert MagickBooleanType to Haskell Bool
isMagickTrue :: MagickBooleanType -> Bool
isMagickTrue = (/= 0)

-- Manage the lifecycle of MagickWandGenesis / MagickWandTerminus
withImageMagickEnv :: MonadIO m => m a -> m a
withImageMagickEnv ioAction = do
  liftIO c_magickWandGenesis
  result <- ioAction
  liftIO c_magickWandTerminus
  return result

-- Create a managed MagickWand using ForeignPtr
newManagedMagickWand :: (MonadIO m, MonadError ImageMagickError m ) => m (ForeignPtr CMagickWand)
newManagedMagickWand = do
  ptr <- liftIO c_newMagickWand
  if ptr == nullPtr
    then liftEither $ Left NullPtrError
    else do
      -- Adapt the finalizer type if necessary. The return type of DestroyMagickWand
      -- is Ptr CMagickWand, but for a finalizer, we usually expect Ptr a -> IO ().
      -- We can cast FunPtr or write a small wrapper if GHC complains about the type.
      -- Let's assume a direct cast works or that FunPtr (Ptr a -> IO b) is acceptable.
      -- A more robust way for the finalizer if types don't match perfectly:
      -- let finalizer :: Ptr CMagickWand -> IO ()
      --     finalizer p = c_destroyMagickWand_funptr_direct p >> return ()
      -- where c_destroyMagickWand_funptr_direct is a dynamic call to c_destroyMagickWand_funptr
      -- However, often GHC is flexible enough with FunPtr (Ptr a -> IO b).
      -- Let's try the direct FunPtr first. If it fails, we'll need to adjust.
      -- The `FunPtr (Ptr a -> IO b)` should be fine if b is ignored.
      foreignPtr <- liftIO $ newForeignPtr c_destroyMagickWand_funptr ptr

      liftEither $ Right foreignPtr
      -- Note: A simpler finalizer FunPtr import would be:
      -- foreign import ccall safe "&DestroyMagickWand"
      --  destroyMagickWandFinalizer :: FunPtr (Ptr CMagickWand -> IO ())
      -- Then use `destroyMagickWandFinalizer` directly in `newForeignPtr`.
      -- This often works because the C function's return is ignored by the finalizer mechanism.

-- Create a managed MagickWand using ForeignPtr
newManagedDrawingWand :: (MonadIO m, MonadError ImageMagickError m ) => m (ForeignPtr CDrawingWand)
newManagedDrawingWand = do
  ptr <- liftIO c_newDrawingWand
  if ptr == nullPtr
    then liftEither $ Left NullPtrError
    else do
      foreignPtr <- liftIO $ newForeignPtr c_destroyDrawingWand_funptr ptr
      liftEither $ Right foreignPtr

-- Read an image into a managed wand
readImage :: (MonadIO m, MonadError ImageMagickError m) => ForeignPtr CMagickWand -> FilePath -> m ()
readImage fwand path = do
  success <- liftIO $ do
    withForeignPtr fwand $ \wandPtr ->
      withCString path $ \cPath -> do
        -- Call the C function to read the image
        c_printf cPath
        result <- c_magickReadImage wandPtr cPath
        -- Check if the result is true (non-zero)
        pure $ isMagickTrue result

  let
    either_result = if success then Right () else Left ReadFileFailure

  liftEither either_result

addQuoteToImage :: (MonadIO m, MonadError ImageMagickError m) => C.AnnotatedQuote -> FilePath -> FilePath -> m ()
addQuoteToImage C.AQuote {..} in_file out_file = withImageMagickEnv $ do

  magick_wand <- newManagedMagickWand
  drawingWand <- newManagedDrawingWand

  _ <- readImage magick_wand in_file

  let quote = T.unpack aQuote
  liftIO $
    withForeignPtr magick_wand $ \magic -> do
    withForeignPtr drawingWand $ \draw -> do
      withCString "Arial" $ \font -> do
        c_drawSetFont draw font
      withCString "12" $ \size -> do
        c_drawSetFontSize draw size
      withCString quote $ \x -> do
        c_drawSetAnnotation draw x
      c_magickDrawImage magic draw
      withCString out_file $ \file_path -> do
        c_magickWriteImage magic file_path

  pure ()
