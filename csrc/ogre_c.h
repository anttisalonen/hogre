#ifndef __AA_OGRE_H
#define __AA_OGRE_H

#include <OGRE/Ogre.h>
#include <CEGUI/CEGUI.h>
#include <OGRE/OgreCEGUIRenderer.h>

extern "C" 
{
int init(float ambient_light_r, float ambient_light_g, float ambient_light_b, 
        int shadow_type, const char* res_filename, int autocreatewindow, const char* title,
        float bg_r, float bg_g, float bg_b);
int newEntity(const char* name, const char* model, int castshadows);
int setEntityPosition(const char* name, float x, float y, float z);
int cleanup();
int render();
void clearScene();
void addLight(const char* lightname, int lighttype, 
        float diffcolor_r, float diffcolor_g, float diffcolor_b, 
        float speccolor_r, float speccolor_g, float speccolor_b, 
        float direction_x, float direction_y, float direction_z,
        float pos_x, float pos_y, float pos_z,
        float range_min_rad, float range_max_rad);
void addPlane(float plane_x, float plane_y, float plane_z, 
        float shift, const char *planename, 
        float size_x, float size_y, int xseg, int yseg, float utile, float vtile, 
        float upvec_x, float upvec_y, float upvec_z, 
        float pos_x, float pos_y, float pos_z, 
        const char *materialname, int castshadows);
void setupCamera(float pos_x, float pos_y, float pos_z, 
        float look_x, float look_y, float look_z, 
        float pitch, float yaw, float roll);
void addEntity(const char* name, const char* mesh, 
        float pos_x, float pos_y, float pos_z, 
        float scale_x, float scale_y, float scale_z, 
        float pitch, float yaw, float roll);
int newEntity(const char* name, const char* model, int castshadows);
int setEntityPosition(const char* name, float x, float y, float z);
void setLightType(const char* lightname, int lighttype);
void setLightDiffuseColor(const char* lightname, float color_r, float color_g, float color_b);
void setLightSpecularColor(const char* lightname, float color_r, float color_g, float color_b);
void setLightDirection(const char* lightname, float x, float y, float z);
void setLightPosition(const char* lightname, float x, float y, float z);
void setSpotlightRange(const char* lightname, float min_rad, float max_rad);
void newLight(const char* lightname);
}

#endif

