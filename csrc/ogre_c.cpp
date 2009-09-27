#include "ogre_c.h"

using namespace Ogre;
using namespace std;

static const char* scenemgrname = "Default SceneManager";

static Root *gRoot;
static SceneManager *gMgr;
static Camera *gCam;
static SceneNode *gCamnode;
static RenderWindow *gRenderWindow;
static CEGUI::OgreCEGUIRenderer *gRenderer;
static CEGUI::System *gSystem;

// Local functions
static void createRoot()
{
    gRoot = new Root();
}

static void defineResources(const char* filename)
{
    string secName, typeName, archName;
    ConfigFile cf;
    cf.load(filename);

    ConfigFile::SectionIterator seci = cf.getSectionIterator();
    while (seci.hasMoreElements())
    {
        secName = seci.peekNextKey();
        ConfigFile::SettingsMultiMap *settings = seci.getNext();
        ConfigFile::SettingsMultiMap::iterator i;
        for (i = settings->begin(); i != settings->end(); ++i)
        {
            typeName = i->first;
            archName = i->second;
            ResourceGroupManager::getSingleton().addResourceLocation(archName, typeName, secName);
        }
    }
}

static void setupRenderSystem()
{
    if (!gRoot->restoreConfig() && !gRoot->showConfigDialog())
        throw Exception(52, "User canceled the config dialog!", "Application::setupRenderSystem()");
}

static void createRenderWindow(int autocreatewindow, const char* windowtitle)
{
    // gRoot->restoreConfig();
    // gRoot->loadPlugin("RenderSystem_GL");
    // gRoot->loadPlugin("Plugin_CgProgramManager");
    // gRoot->loadPlugin("Plugin_OctreeSceneManager");

    // gRoot->setRenderSystem(*(gRoot->getAvailableRenderers().begin()));
    if(autocreatewindow)
    {
        gRenderWindow = gRoot->initialise(autocreatewindow, windowtitle);
        return;
    }
    gRoot->restoreConfig();
    gRoot->initialise(false);
    Ogre::NameValuePairList misc;
    misc["currentGLContext"] = String("True");
    gRenderWindow = gRoot->createRenderWindow("MainRenderWindow", 640, 480, false, &misc);
    gRenderWindow->setVisible(true);

    // parseWindowGeometry(gRoot->getRenderSystem()->getConfigOptions(), width, height, fullscreen);
    /*
    gRoot->restoreConfig(false);
    ConfigFile::SettingsMultiMap *misc;
    misc["currentGLContext"] = String("True");
    RenderWindow *renderWindow = gRoot->createRenderWindow("MainRenderWindow", 640, 480, false, &misc);
    renderWindow->setVisible(true);
    */
}

static void initializeResourceGroups()
{
    TextureManager::getSingleton().setDefaultNumMipmaps(5);
    ResourceGroupManager::getSingleton().initialiseAllResourceGroups();
}

static void initCamera(float bg_r, float bg_g, float bg_b)
{
    std::cerr << "create camera" << std::endl;
    gCam = gMgr->createCamera("Camera");
    std::cerr << "add viewport" << std::endl;
    // Viewport *vp = gRoot->getAutoCreatedWindow()->addViewport(gCam);
    Viewport *vp = gRenderWindow->addViewport(gCam);

    std::cerr << "set background color" << std::endl;
    vp->setBackgroundColour(ColourValue(bg_r, bg_g, bg_b));
    std::cerr << "set clip" << std::endl;
    gCam->setNearClipDistance(0.5);
    std::cerr << "set ratio" << std::endl;
    gCam->setAutoAspectRatio(true);

    std::cerr << "create node" << std::endl;
    gCamnode = gMgr->getRootSceneNode()->createChildSceneNode("camnode0");
    std::cerr << "attach" << std::endl;
    gCamnode->attachObject(gCam);
}

static void setupCEGUI()
{
    // RenderWindow *win = gRoot->getAutoCreatedWindow();

    gRenderer = new CEGUI::OgreCEGUIRenderer(gRenderWindow, RENDER_QUEUE_OVERLAY, false, 3000, gMgr);
    gSystem = new CEGUI::System(gRenderer);
}

static void setupScene(float ambient_light_r, float ambient_light_g, float ambient_light_b, int shadow_type)
{
    gMgr = gRoot->createSceneManager(ST_GENERIC, scenemgrname);

    gMgr->setAmbientLight(ColourValue( ambient_light_r, ambient_light_g, ambient_light_b) );
    gMgr->setShadowTechnique(
            shadow_type == 0 ? SHADOWTYPE_NONE : 
            shadow_type == 1 ? SHADOWTYPE_STENCIL_MODULATIVE : 
            shadow_type == 2 ? SHADOWTYPE_STENCIL_ADDITIVE : 
            shadow_type == 3 ? SHADOWTYPE_TEXTURE_MODULATIVE : 
            shadow_type == 4 ? SHADOWTYPE_TEXTURE_ADDITIVE : 
            shadow_type == 5 ? SHADOWTYPE_TEXTURE_ADDITIVE_INTEGRATED : 
                               SHADOWTYPE_TEXTURE_MODULATIVE_INTEGRATED);
}

// Exported functions

// Initialization functions
int init(float ambient_light_r, float ambient_light_g, float ambient_light_b, 
        int shadow_type, const char* res_filename, int autocreatewindow, const char* title, 
        float bg_r, float bg_g, float bg_b)
{
    createRoot();
    defineResources(res_filename);
    setupRenderSystem();
    std::cerr << "!!! creating render window" << std::endl;
    createRenderWindow(autocreatewindow, title);
    std::cerr << "!!! init resource groups" << std::endl;
    initializeResourceGroups();
    std::cerr << "!!! setup scene" << std::endl;
    setupScene(ambient_light_r, ambient_light_g, ambient_light_b, shadow_type);
    std::cerr << "!!! init camera" << std::endl;
    initCamera(bg_r, bg_g, bg_b);
    std::cerr << "!!! setting up cegui" << std::endl;
    setupCEGUI();
    std::cerr << "!!! finished" << std::endl;
    return 0;
}

// Miscellaneous functions
int cleanup()
{
    return 0;
}

int render()
{
    return !(gRoot->renderOneFrame());
}

// Manipulation functions
int newEntity(const char* name, const char* model, int castshadows)
{
    std::stringstream nname;
    nname << "Node_" << name;
    Entity* ent = gMgr->createEntity (name, model);
    ent->setCastShadows(castshadows);
    SceneNode* node = gMgr->getRootSceneNode()->createChildSceneNode(nname.str());
    node->attachObject(ent);
    return 0;
}

int setEntityPosition(const char* name, float x, float y, float z)
{
    gMgr->getEntity(name)->getParentNode()->setPosition(Vector3(x, y, z));
    return 0;
}

void setLightType(const char* lightname, int lighttype)
{
    gMgr->getLight(lightname)->setType(lighttype == 0 ? Light::LT_POINT : lighttype == 1 ? Light::LT_DIRECTIONAL : Light::LT_SPOTLIGHT);
}

void setLightDiffuseColor(const char* lightname, float color_r, float color_g, float color_b)
{
    gMgr->getLight(lightname)->setDiffuseColour(color_r, color_g, color_b);
}

void setLightSpecularColor(const char* lightname, float color_r, float color_g, float color_b)
{
    gMgr->getLight(lightname)->setSpecularColour(color_r, color_g, color_b);
}

void setLightDirection(const char* lightname, float x, float y, float z)
{
    gMgr->getLight(lightname)->setDirection(x, y, z);
}

void setLightPosition(const char* lightname, float x, float y, float z)
{
    gMgr->getLight(lightname)->setPosition(x, y, z);
}

void setSpotlightRange(const char* lightname, float min_rad, float max_rad)
{
    gMgr->getLight(lightname)->setSpotlightRange(Ogre::Radian(min_rad), Ogre::Radian(max_rad));
}

void newLight(const char* lightname)
{
    gMgr->createLight(lightname);
}

void addLight(const char* lightname, int lighttype, 
        float diffcolor_r, float diffcolor_g, float diffcolor_b, 
        float speccolor_r, float speccolor_g, float speccolor_b, 
        float direction_x, float direction_y, float direction_z,
        float pos_x, float pos_y, float pos_z,
        float range_min_rad, float range_max_rad)
{
    Light::LightTypes type = lighttype == 0 ? Light::LT_POINT : lighttype == 1 ? Light::LT_DIRECTIONAL : Light::LT_SPOTLIGHT;
    Light* light;
    light = gMgr->createLight(lightname);
    light->setType(type);
    light->setDiffuseColour(diffcolor_r, diffcolor_g, diffcolor_b);
    light->setSpecularColour(speccolor_r, speccolor_g, speccolor_b);
    if(type != Light::LT_POINT)
        light->setDirection(direction_x, direction_y, direction_z);
    if(type != Light::LT_DIRECTIONAL)
        light->setPosition(Vector3(pos_x, pos_y, pos_z));
    if(type == Light::LT_SPOTLIGHT)
        light->setSpotlightRange(Ogre::Radian(range_min_rad), Ogre::Radian(range_max_rad));
}

void addPlane(float plane_x, float plane_y, float plane_z, 
        float shift, const char *planename, 
        float size_x, float size_y, int xseg, int yseg, float utile, float vtile, 
        float upvec_x, float upvec_y, float upvec_z, 
        float pos_x, float pos_y, float pos_z, 
        const char *materialname, int castshadows)
{
    Plane plane (Vector3(plane_x, plane_y, plane_z), shift);
    MeshManager::getSingleton().createPlane(
        "ground", 
        ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME, 
        plane, 
        size_x, size_y, xseg, yseg, 
        true, 1, utile, vtile, Vector3(upvec_x, upvec_y, upvec_z));
    Entity *plent = gMgr->createEntity(planename, "ground");
    SceneNode* plnode = gMgr->getRootSceneNode()->createChildSceneNode();
    plnode->attachObject(plent);
    plnode->setPosition(Vector3(pos_x, pos_y, pos_z));
    plent->setMaterialName(materialname);
    plent->setCastShadows(castshadows);
}

void clearScene()
{
    gMgr->clearScene();
}

void setupCamera(float pos_x, float pos_y, float pos_z, 
        float look_x, float look_y, float look_z, 
        float roll)
{
    gCam->setPosition (Vector3(pos_x, pos_y, pos_z));
    // gCamnode->lookAt (Vector3(look_x, look_y, look_z), Node::TS_WORLD);
    gCam->lookAt(look_x, look_y, look_z);
    // gCam->yaw(Radian(yaw));
    // gCam->pitch(Radian(pitch));
    if(abs(roll) > 0.0001f)
        gCam->roll(Radian(roll));
}

void addEntity(const char* name, const char* mesh, 
        float pos_x, float pos_y, float pos_z, 
        float scale_x, float scale_y, float scale_z, 
        float pitch, float yaw, float roll)
{
    std::stringstream nname;
    nname << name << "Node";
    Entity *ent1 = gMgr->createEntity (name, mesh);
    SceneNode *node1 = gMgr->getRootSceneNode()->createChildSceneNode(nname.str());
    node1->setPosition(Vector3(pos_x, pos_y, pos_z));
    node1->setScale(scale_x, scale_y, scale_z);
    node1->yaw(Radian(yaw));
    node1->pitch(Radian(pitch));
    node1->roll(Radian(roll));
    node1->attachObject(ent1);
}


