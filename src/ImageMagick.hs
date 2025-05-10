{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module ImageMagick
  (  addQuoteToImage
  ) where

-- Modules
import Foreign.C.String   (CString, withCString)
import Foreign.C.Types    (CSize(..), CUInt(..))
import Foreign.Ptr        (Ptr, nullPtr, FunPtr, castFunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)

import Control.Monad.Except (MonadError, MonadIO, liftIO, liftEither)

import qualified Clippings as C

-- Left Error Type
data ImageMagickError =
  NullPtrError
  | NoImageDimensionsError
  | ReadFileFailure
  | OtherError String

-- Opaque type for MagickWand*
data CMagickWand

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

-- MagickWand *DestroyMagickWand(MagickWand *wand);
-- The API returns the destroyed wand, often NULL. We only care about the side effect.
-- We need a FunPtr for the finalizer.
foreign import ccall safe "&DestroyMagickWand"
  c_destroyMagickWand_funptr :: FunPtr (Ptr CMagickWand -> IO (Ptr CMagickWand))
  -- Simpler signature if we ignore the return for the finalizer context:
  -- c_destroyMagickWand_funptr :: FunPtr (Ptr CMagickWand -> IO ())

-- MagickBooleanType MagickReadImage(MagickWand *wand, const char *filename);
foreign import ccall safe "MagickReadImage"
  c_magickReadImage :: Ptr CMagickWand -> CString -> IO MagickBooleanType

-- size_t MagickGetImageWidth(MagickWand *wand);
foreign import ccall safe "MagickGetImageWidth"
  c_magickGetImageWidth :: Ptr CMagickWand -> IO CSize

-- size_t MagickGetImageHeight(MagickWand *wand);
foreign import ccall safe "MagickGetImageHeight"
  c_magickGetImageHeight :: Ptr CMagickWand -> IO CSize

-- char *MagickGetException(const MagickWand *wand, ExceptionType *severity);
-- For simplicity, we won't use this directly for detailed error messages,
-- but it's good to be aware of. We'd need to define ExceptionType too.
-- foreign import ccall unsafe "MagickGetException"
--   c_magickGetException :: Ptr CMagickWand -> Ptr CInt -> IO CString


-- --- Haskell Wrappers ---

-- Helper to convert MagickBooleanType to Haskell Bool
isMagickTrue :: MagickBooleanType -> Bool
isMagickTrue = (/= 0)

-- Manage the lifecycle of MagickWandGenesis / MagickWandTerminus
withImageMagickEnv :: MonadIO m => m a -> m a
withImageMagickEnv io_action = do
  liftIO c_magickWandGenesis
  result <- io_action
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
      foreign_ptr <- liftIO $ newForeignPtr (castFunPtr c_destroyMagickWand_funptr :: FunPtr (Ptr CMagickWand -> IO ())) ptr

      liftEither $ Right foreign_ptr
      -- Note: A simpler finalizer FunPtr import would be:
      -- foreign import ccall safe "&DestroyMagickWand"
      --  destroyMagickWandFinalizer :: FunPtr (Ptr CMagickWand -> IO ())
      -- Then use `destroyMagickWandFinalizer` directly in `newForeignPtr`.
      -- This often works because the C function's return is ignored by the finalizer mechanism.

-- Read an image into a managed wand
readImage :: (MonadIO m, MonadError ImageMagickError m) => ForeignPtr CMagickWand -> FilePath -> m ()
readImage fwand path = do
  success <- liftIO $ do
    withForeignPtr fwand $ \wandPtr ->
      withCString path $ \cPath -> do
        -- Call the C function to read the image
        result <- c_magickReadImage wandPtr cPath
        -- Check if the result is true (non-zero)
        pure $ isMagickTrue result

  let
    either_result = if success then Right () else Left ReadFileFailure

  liftEither either_result

-- -- Get image dimensions
-- getImageDimensions :: (MonadIO m, MonadError ImageMagickError m) => ForeignPtr CMagickWand -> m (Int, Int)
-- getImageDimensions fwand = do
--   either_result <- liftIO $ withForeignPtr fwand $ \wandPtr -> do
--       -- It's good practice to check if an image is loaded,
--       -- but MagickGetImageWidth/Height might just return 0 or error internally.
--       -- For simplicity, we'll call them directly.
--       width  <- c_magickGetImageWidth wandPtr
--       height <- c_magickGetImageHeight wandPtr
--       -- Basic check: if width or height is 0, maybe something went wrong or no image.
--       if width == 0 && height == 0
--           then pure $ Left NoImageDimensionsError
--           else pure $ Right (fromIntegral width, fromIntegral height)
--
--   liftEither either_result
--

addQuoteToImage :: (MonadIO m, MonadError ImageMagickError m) => C.AnnotatedQuote -> FilePath -> m ()
addQuoteToImage C.AQuote {..} image_file = withImageMagickEnv $ do

  wand <- newManagedMagickWand

  _ <- readImage wand image_file

  pure ()
