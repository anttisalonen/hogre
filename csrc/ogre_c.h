#ifndef __AA_OGRE_H
#define __AA_OGRE_H

#include <OGRE/Ogre.h>

extern "C" 
{
int init(int shadow_type, const char* res_filename, int autocreatewindow, const char* title,
        float bg_r, float bg_g, float bg_b, int manager_type);
void setAmbientLight(float ambient_light_r, float ambient_light_g, float ambient_light_b);
int newEntity(const char* name, const char* model, int castshadows);
void setEntityPosition(const char* name, float x, float y, float z);
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
        float roll);
void addEntity(const char* name, const char* mesh, 
        float pos_x, float pos_y, float pos_z, 
        float scale_x, float scale_y, float scale_z, 
        float pitch, float yaw, float roll);
int newEntity(const char* name, const char* model, int castshadows);
void setLightType(const char* lightname, int lighttype);
void setLightDiffuseColor(const char* lightname, float color_r, float color_g, float color_b);
void setLightSpecularColor(const char* lightname, float color_r, float color_g, float color_b);
void setLightDirection(const char* lightname, float x, float y, float z);
void setLightPosition(const char* lightname, float x, float y, float z);
void setSpotlightRange(const char* lightname, float min_rad, float max_rad);
void newLight(const char* lightname);
void rotateEntity(const char* name, float yaw, float pitch, float roll, int space);
void rotateCamera(float yaw, float pitch, float roll, int space);
void translateEntity(const char* name, float x, float y, float z, int space);
void translateCamera(float x, float y, float z);
void setLightVisible(const char* name, int vis);
void setSkyDome(int enabled, const char* texture, float curvature);
void setWorldGeometry(const char* cfg);
void setCameraPosition(float x, float y, float z);
void getCameraPosition(float* x, float* y, float* z);
int raySceneQuerySimple(float orig_x, float orig_y, float orig_z, 
                float dir_x, float dir_y, float dir_z,
                float* res_x, float* res_y, float* res_z);
int raySceneQueryMouseSimple (float xpos, float ypos, 
                float* res_x, float* res_y, float* res_z);
}

#endif

