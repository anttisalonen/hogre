-- | This module includes the necessary functionality for using OGRE from
--   Haskell. Note that you need OGRE and CEGUI libraries and headers.
--   Currently, only OGRE version 1.7.0dev-unstable (Cthugha) has been tested.
--   Usage for a simple scene creation:
--
--   1. Create the settings structure 'OgreSettings'.
--
--   2. Define your scene by building up an 'OgreScene'.
--
--   3. 'initOgre' using OgreSettings and OgreScene.
--
--   4. Call 'renderOgre' in a loop.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# INCLUDE <ogre.h> #-}
module Graphics.Ogre.Ogre(Vector3(..),
        Angle, Color(..),
        ShadowTechnique(..),
        Light(..),
        EntityType(..),
        Camera(..),
        Entity(..),
        OgreSettings(..),
        OgreScene(..),
        Ogre(..),
        halfPI,
        degToRad,
        unitX, unitY, unitZ,
        initOgre,
        addCamera,
        addLight,
        addEntity,
        setEntityPosition,
        renderOgre,
        cleanupOgre)
where

import CTypes
import CString

-- C imports
foreign import ccall "ogre.h init" c_init :: CFloat -> CFloat -> CFloat -> CInt -> CString -> CInt -> CString -> CFloat -> CFloat -> CFloat -> IO ()
-- foreign import ccall "ogre.h newEntity" c_new_entity :: CString -> CString -> CInt -> IO ()
foreign import ccall "ogre.h setEntityPosition" c_set_entity_position :: CString -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "ogre.h cleanup" c_cleanup :: IO ()
foreign import ccall "ogre.h render" c_render :: IO ()
foreign import ccall "ogre.h addEntity" c_add_entity :: CString -> CString -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "ogre.h setupCamera" c_setup_camera :: CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "ogre.h addPlane" c_add_plane :: CFloat -> CFloat -> CFloat -> CFloat -> CString -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CString -> CInt -> IO ()
foreign import ccall "ogre.h addLight" c_add_light :: CString -> CInt -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()

-- Primitive data types
data Vector3 = Vector3 { x :: Float, y :: Float, z :: Float }
    deriving (Eq, Show, Read)

type Angle = Float

data Color = Color { r :: Float, g :: Float, b :: Float }
    deriving (Eq, Show, Read)

-- Ogre-specific data types
data ShadowTechnique = None
                     | TextureModulative
                     | StencilModulative  -- ^ Note: as of 0.0.1, stencil shadows
                                          -- do not work when the window was created
                                          -- by SDL.
                     | StencilAdditive
    deriving (Eq, Show, Read, Enum)

data Light = SpotLight { spotlightname     :: String
                       , spotlightposition :: Vector3
                       , diffuse           :: Color
                       , specular          :: Color
                       , direction         :: Vector3
                       , range             :: (Angle, Angle)
                       }
    deriving (Eq, Show, Read)

-- | See <http://www.ogre3d.org/docs/api/html/classOgre_1_1Plane.html>
data EntityType = Plane { normal      :: Vector3
                        , shift       :: Float
                        , width       :: Float
                        , height      :: Float
                        , xsegments   :: Int
                        , ysegments   :: Int
                        , utile       :: Float
                        , vtile       :: Float
                        , upvector    :: Vector3
                        , material    :: String
                        }
                | StdMesh { mesh        :: String 
                          , pitch       :: Angle
                          , roll        :: Angle
                          , yaw         :: Angle
                          }
    deriving (Eq, Show, Read)

data Camera = Camera { lookat      :: Vector3
                     , camroll     :: Angle
                     , camposition :: Vector3
                     }
    deriving (Eq, Show, Read)

data Entity = Entity { name        :: String
                     , position    :: Vector3
                     , entitytype  :: EntityType
                     , castshadows :: Bool
                     , scale       :: Vector3
                     }
    deriving (Eq, Show, Read)

-- Main Ogre data types
-- | General, scene-wide Ogre settings.
data OgreSettings = OgreSettings { resourcefile     :: FilePath          -- ^ Path to resources.cfg.
                                 , autocreatewindow :: Bool              -- ^ Whether the window should 
                                                                         -- be created automatically (by Ogre).
                                                                         -- Turn this off if the window should
                                                                         -- be created by another library
                                                                         -- (e.g.) SDL.
                                 , caption          :: String            -- ^ Window caption.
                                 , ambientlight     :: Color
                                 , shadowtechnique  :: ShadowTechnique
                                 }
    deriving (Eq, Show, Read)

data OgreScene = OgreScene { camera          :: Camera
                           , entities        :: [Entity]
                           , lights          :: [Light]
                           }
    deriving (Eq, Show, Read)

-- | Main configuration structure for Ogre.
data Ogre = Ogre { settings :: OgreSettings
                 , scene    :: OgreScene     -- ^ Entities and other objects that define the scene.
                 }
    deriving (Eq, Show, Read)

-- Helpers
halfPI :: Float
halfPI = pi * 0.5

degToRad :: Float -> Float
degToRad d = d * pi / 180.0

unitX :: Vector3
unitX = Vector3 1.0 0.0 0.0

unitY :: Vector3
unitY = Vector3 0.0 1.0 0.0

unitZ :: Vector3
unitZ = Vector3 0.0 0.0 1.0

-- | Initializes Ogre with given settings. This must be called before manipulating or rendering the scene.
initOgre :: Ogre -> IO ()
initOgre (Ogre sett scen) = do
    let amb = ambientlight sett
    withCString (resourcefile sett) $ \c_res -> do
    withCString (caption sett) $ \c_caption -> do
    c_init (realToFrac (r amb)) (realToFrac (g amb)) (realToFrac (b amb)) ((fromIntegral . fromEnum) (shadowtechnique sett)) c_res ((fromIntegral . fromEnum) (autocreatewindow sett)) c_caption 0.0 0.0 0.0
    addCamera (camera scen)
    mapM_ addLight (lights scen)
    mapM_ addEntity (entities scen)

addCamera :: Camera -> IO ()
addCamera (Camera look rol pos) = do
    c_setup_camera (realToFrac (x pos)) (realToFrac (y pos)) (realToFrac (z pos)) (realToFrac (x look)) (realToFrac (y look)) (realToFrac (z look)) 0.0 0.0 (realToFrac rol)

addLight :: Light -> IO ()
addLight (SpotLight nam pos dif spec dir (rmin, rmax)) = do
    withCString nam $ \c_name -> do
    c_add_light c_name 2 (realToFrac (r dif)) (realToFrac (g dif)) (realToFrac (b dif)) (realToFrac (r spec)) (realToFrac (g spec)) (realToFrac (b spec)) (realToFrac (x dir)) (realToFrac (y dir)) (realToFrac (z dir)) (realToFrac (x pos)) (realToFrac (y pos)) (realToFrac (z pos)) (realToFrac rmin) (realToFrac rmax)

addEntity :: Entity -> IO ()
addEntity (Entity n pos t sh sc) = do
    withCString n $ \c_name -> do
    case t of
      StdMesh m pit rol ya -> do
        withCString m $ \c_meshname -> do
        c_add_entity c_name c_meshname (realToFrac (x pos)) (realToFrac (y pos)) (realToFrac (z pos)) (realToFrac (x sc)) (realToFrac (y sc)) (realToFrac (z sc)) (realToFrac pit) (realToFrac rol) (realToFrac ya)
      Plane norm shif wid hei xseg yseg ut vt upv mat -> do
        withCString mat $ \c_material -> do
        c_add_plane (realToFrac (x norm)) (realToFrac (y norm)) (realToFrac (z norm)) (realToFrac (shif)) c_name (realToFrac (wid)) (realToFrac (hei)) (fromIntegral xseg) (fromIntegral (yseg)) (realToFrac (ut)) (realToFrac (vt)) (realToFrac (x upv)) (realToFrac (y upv)) (realToFrac (z upv)) (realToFrac (x pos)) (realToFrac (y pos)) (realToFrac (z pos)) c_material ((fromIntegral . fromEnum) sh)

setEntityPosition :: String    -- ^ Name of entity
                  -> Vector3   -- ^ New position
                  -> IO ()
setEntityPosition n (Vector3 x_ y_ z_) = withCString n $ \cn -> c_set_entity_position cn (realToFrac x_) (realToFrac y_) (realToFrac z_)

-- | 'renderOgre' renders one frame.
renderOgre :: IO ()
renderOgre = c_render

cleanupOgre :: IO ()
cleanupOgre = c_cleanup

