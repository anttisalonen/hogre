-- | This module includes the necessary functionality for using OGRE from
--   Haskell. Note that you need OGRE and CEGUI libraries and headers.
--   Currently, only OGRE version 1.7.0dev-unstable (Cthugha) has been tested.
--   Usage for a simple scene creation:
--
--   1. Create the settings structure 'OgreSettings'.
--
--   2. Define your scene by building up an 'OgreScene'.
--
--   3. call 'initOgre' using your OgreSettings.
--
--   4. add your scene using 'addScene'.
--
--   5. Call 'renderOgre' in a loop.
--
--   6. To ensure clean shutdown, call 'cleanupOgre'.
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# INCLUDE <ogre.h> #-}
module Graphics.Ogre.Ogre(Vector3(..),
        Angle, Rotation(..), Color(..),
        TransformSpace(..),
        ShadowTechnique(..),
        Light(..),
        LightType(..),
        EntityType(..),
        Camera(..),
        Entity(..),
        OgreSettings(..),
        OgreScene(..),
        SceneManagerType(..),
        halfPI,
        degToRad,
        unitX, unitY, unitZ,
        negUnitX, negUnitY, negUnitZ,
        initOgre,
        addScene,
        setupCamera,
        clearScene,
        addLight,
        addEntity,
        setLightPosition,
        setEntityPosition,
        rotateEntity,
        rotateCamera,
        translateEntity,
        translateCamera,
        setLightVisible,
        setAmbientLight,
        setSkyDome,
        setWorldGeometry,
        renderOgre,
        cleanupOgre)
where

import CTypes
import CString

-- C imports
foreign import ccall "ogre.h init" c_init :: CInt -> CString -> CInt -> CString -> CFloat -> CFloat -> CFloat -> CInt -> IO ()
foreign import ccall "ogre.h setAmbientLight" c_set_ambient_light :: CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "ogre.h setLightPosition" c_set_light_position :: CString -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "ogre.h setEntityPosition" c_set_entity_position :: CString -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "ogre.h cleanup" c_cleanup :: IO ()
foreign import ccall "ogre.h render" c_render :: IO ()
foreign import ccall "ogre.h addEntity" c_add_entity :: CString -> CString -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "ogre.h setupCamera" c_setup_camera :: CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "ogre.h addPlane" c_add_plane :: CFloat -> CFloat -> CFloat -> CFloat -> CString -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CString -> CInt -> IO ()
foreign import ccall "ogre.h addLight" c_add_light :: CString -> CInt -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "ogre.h rotateEntity" c_rotate_entity :: CString -> CFloat -> CFloat -> CFloat -> CInt -> IO ()
foreign import ccall "ogre.h rotateCamera" c_rotate_camera :: CFloat -> CFloat -> CFloat -> CInt -> IO ()
foreign import ccall "ogre.h translateEntity" c_translate_entity :: CString -> CFloat -> CFloat -> CFloat -> CInt -> IO ()
foreign import ccall "ogre.h translateCamera" c_translate_camera :: CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "ogre.h setLightVisible" c_set_light_visible :: CString -> CInt -> IO ()
foreign import ccall "ogre.h setSkyDome" c_set_sky_dome :: CInt -> CString -> CFloat -> IO ()
foreign import ccall "ogre.h setWorldGeometry" c_set_world_geometry :: CString -> IO ()
foreign import ccall "ogre.h clearScene" c_clear_scene :: IO ()

-- Primitive data types
data Vector3 = Vector3 { x :: Float, y :: Float, z :: Float }
    deriving (Eq, Show, Read)

type Angle = Float

data TransformSpace = Local
                    | Parent
                    | World
    deriving (Eq, Show, Read, Enum)

data Rotation = YPR { yaw   :: Angle
                    , pitch :: Angle
                    , roll  :: Angle
                    }
    deriving (Eq, Show, Read)

data Color = Color { r :: Float, g :: Float, b :: Float }
    deriving (Eq, Show, Read)

-- Ogre-specific data types
data ShadowTechnique = None
                     | StencilModulative  -- ^ Note: as of 0.0.1, stencil shadows
                                          -- do not work when the window was created
                                          -- by SDL.
                     | StencilAdditive
                     | TextureModulative
                     | TextureAdditive
                     | TextureAdditiveIntegrated
                     | TextureModulativeIntegrated
    deriving (Eq, Show, Read, Enum)

data Light = Light { lightname :: String
                   , diffuse   :: Color
                   , specular  :: Color
                   , lighttype :: LightType
                   }
    deriving (Eq, Show, Read)

data LightType = PointLight       { plposition  :: Vector3 }
               | DirectionalLight { dldirection :: Vector3 }
               | SpotLight        { slposition  :: Vector3
                                  , sldirection :: Vector3
                                  , range       :: (Angle, Angle)
                                  }
    deriving (Eq, Show, Read)

-- | See <http://www.ogre3d.org/docs/api/html/classOgre_1_1Plane.html> and Ogre::MeshManager::createPlane: <http://www.ogre3d.org/docs/api/html/classOgre_1_1MeshManager.html>
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
                          , rotation    :: Rotation
                          }
    deriving (Eq, Show, Read)

data Camera = Camera { lookat      :: Vector3
                     , camroll     :: Angle
                     , camposition :: Vector3
                     }
    deriving (Eq, Show, Read)

defaultCamera :: Camera
defaultCamera = Camera (Vector3 1.0 0.0 0.0) 0 (Vector3 0.0 0.0 0.0)

data Entity = Entity { name        :: String
                     , position    :: Vector3
                     , entitytype  :: EntityType
                     , castshadows :: Bool
                     , scale       :: Vector3
                     }
    deriving (Eq, Show, Read)

data SceneManagerType = Generic
                      | ExteriorClose
                      | ExteriorFar
                      | ExteriorRealFar
                      | Interior
    deriving (Eq, Show, Read)

-- Main Ogre data types
-- | General, scene-wide Ogre settings. Main configuration structure for Ogre.
data OgreSettings = OgreSettings { resourcefile     :: FilePath          -- ^ Path to resources.cfg.
                                 , autocreatewindow :: Bool              -- ^ Whether the window should 
                                                                         -- be created automatically (by Ogre).
                                                                         -- Turn this off if the window should
                                                                         -- be created by another library
                                                                         -- (e.g.) SDL.
                                 , caption          :: String            -- ^ Window caption.
                                 , ambientlight     :: Color
                                 , shadowtechnique  :: ShadowTechnique
                                 , scenemanagertype :: [SceneManagerType]
                                 }
    deriving (Eq, Show, Read)

-- | Entities and other objects that define the scene.
data OgreScene = OgreScene { camera          :: Camera
                           , entities        :: [Entity]
                           , lights          :: [Light]
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

nullVector :: Vector3
nullVector = Vector3 0.0 0.0 0.0

negUnitX :: Vector3
negUnitX = Vector3 (-1.0) 0.0 0.0

negUnitY :: Vector3
negUnitY = Vector3 0.0 (-1.0) 0.0

negUnitZ :: Vector3
negUnitZ = Vector3 0.0 0.0 (-1.0)

managerMaskFromEnum :: [SceneManagerType] -> CInt
managerMaskFromEnum = foldl go 0
    where go acc s = acc + go' s
          go' Generic          = 1
          go' ExteriorClose    = 2
          go' ExteriorFar      = 4
          go' ExteriorRealFar  = 8
          go' Interior         = 16

-- | Initializes Ogre with given settings. This must be called before manipulating or rendering the scene.
initOgre :: OgreSettings -> IO ()
initOgre sett = do
    let mgr_type = min 1 (managerMaskFromEnum (scenemanagertype sett))
    withCString (resourcefile sett) $ \c_res -> do
    withCString (caption sett) $ \c_caption -> do
    c_init ((fromIntegral . fromEnum) (shadowtechnique sett)) c_res ((fromIntegral . fromEnum) (autocreatewindow sett)) c_caption 0.0 0.0 0.0 mgr_type
    setAmbientLight (ambientlight sett)
    setupCamera defaultCamera

setAmbientLight :: Color -> IO ()
setAmbientLight (Color r_ g_ b_) = c_set_ambient_light (realToFrac r_) (realToFrac g_) (realToFrac b_)

clearScene :: IO ()
clearScene = c_clear_scene

addScene :: OgreScene -> IO ()
addScene scen = do
    setupCamera (camera scen)
    mapM_ addLight (lights scen)
    mapM_ addEntity (entities scen)

setupCamera :: Camera -> IO ()
setupCamera (Camera look rol pos) = do
    c_setup_camera 
        (realToFrac (x pos)) 
        (realToFrac (y pos)) 
        (realToFrac (z pos)) 
        (realToFrac (x look)) 
        (realToFrac (y look)) 
        (realToFrac (z look)) 
        (realToFrac rol)

addLight :: Light -> IO ()
addLight (Light nam dif spec ltype) = do
    let (t, pos, dir, rmin, rmax) = case ltype of
          PointLight       lp                 -> (0, lp, nullVector, 0, 0)
          DirectionalLight ld                 -> (1, nullVector, ld, 0, 0)
          SpotLight        lp ld (lrmi, lrma) -> (2, lp, ld, lrmi, lrma)
    withCString nam $ \c_name -> do
      c_add_light c_name t
        (realToFrac (r dif)) 
        (realToFrac (g dif)) 
        (realToFrac (b dif)) 
        (realToFrac (r spec)) 
        (realToFrac (g spec)) 
        (realToFrac (b spec)) 
        (realToFrac (x dir)) 
        (realToFrac (y dir)) 
        (realToFrac (z dir)) 
        (realToFrac (x pos)) 
        (realToFrac (y pos)) 
        (realToFrac (z pos)) 
        (realToFrac rmin) 
        (realToFrac rmax)

addEntity :: Entity -> IO ()
addEntity (Entity n pos t sh sc) = do
    withCString n $ \c_name -> do
    case t of
      StdMesh m (YPR ya pit ro) -> do
        withCString m $ \c_meshname -> do
        c_add_entity c_name c_meshname 
                (realToFrac (x pos)) 
                (realToFrac (y pos)) 
                (realToFrac (z pos)) 
                (realToFrac (x sc)) 
                (realToFrac (y sc)) 
                (realToFrac (z sc)) 
                (realToFrac pit) (realToFrac ya) (realToFrac ro)
      Plane norm shif wid hei xseg yseg ut vt upv mat -> do
        withCString mat $ \c_material -> do
        c_add_plane (realToFrac (x norm)) (realToFrac (y norm)) (realToFrac (z norm)) (realToFrac (shif)) c_name (realToFrac (wid)) (realToFrac (hei)) (fromIntegral xseg) (fromIntegral (yseg)) (realToFrac (ut)) (realToFrac (vt)) (realToFrac (x upv)) (realToFrac (y upv)) (realToFrac (z upv)) (realToFrac (x pos)) (realToFrac (y pos)) (realToFrac (z pos)) c_material ((fromIntegral . fromEnum) sh)

setLightPosition :: String    -- ^ Name of light
                 -> Vector3   -- ^ New position
                 -> IO ()
setLightPosition n (Vector3 x_ y_ z_) = withCString n $ \cn -> c_set_light_position cn (realToFrac x_) (realToFrac y_) (realToFrac z_)

setEntityPosition :: String    -- ^ Name of entity
                  -> Vector3   -- ^ New position
                  -> IO ()
setEntityPosition n (Vector3 x_ y_ z_) = withCString n $ \cn -> c_set_entity_position cn (realToFrac x_) (realToFrac y_) (realToFrac z_)

rotateEntity :: String -> Rotation -> TransformSpace -> IO ()
rotateEntity n (YPR ya pit ro) ts = withCString n $ \cn -> c_rotate_entity cn (realToFrac ya) (realToFrac pit) (realToFrac ro) ((fromIntegral . fromEnum) ts)

rotateCamera :: Rotation -> TransformSpace -> IO ()
rotateCamera (YPR ya pit ro) ts = c_rotate_camera (realToFrac ya) (realToFrac pit) (realToFrac ro) ((fromIntegral . fromEnum) ts)

translateEntity :: String -> Vector3 -> TransformSpace -> IO ()
translateEntity n (Vector3 x_ y_ z_) ts = withCString n $ \cn -> c_translate_entity cn (realToFrac x_) (realToFrac y_) (realToFrac z_) ((fromIntegral . fromEnum) ts)

translateCamera :: Vector3 -> IO ()
translateCamera (Vector3 x_ y_ z_) = c_translate_camera (realToFrac x_) (realToFrac y_) (realToFrac z_)

setLightVisible :: String -> Bool -> IO ()
setLightVisible n v = withCString n $ \cn -> c_set_light_visible cn ((fromIntegral . fromEnum) v)

-- | See Ogre::SceneManager::setSkyDome().
setSkyDome :: Maybe (String, Float) -- ^ If Nothing, will disable sky dome. 
                                    -- Otherwise, String refers to the 
                                    -- material name. Float defines the
                                    -- curvature. 
           -> IO ()
setSkyDome Nothing = withCString "" $ \cs -> c_set_sky_dome 0 cs 5
setSkyDome (Just (n, curv)) = withCString n $ \cs -> c_set_sky_dome 1 cs (realToFrac curv)

-- | See Ogre::SceneManager::setWorldGeometry().
setWorldGeometry :: String -> IO ()
setWorldGeometry s = withCString s $ \cs -> c_set_world_geometry cs

-- | 'renderOgre' renders one frame.
renderOgre :: IO ()
renderOgre = c_render

cleanupOgre :: IO ()
cleanupOgre = c_cleanup

