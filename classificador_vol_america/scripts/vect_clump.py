from qgis.utils import iface
from PyQt5.QtCore import QVariant

_AREA_FIELD = 'area'
_VALUE_FIELD = 'DN'

# Names of the new fields to be added to the layer
_NEW_AREA_FIELD = 'intArea'
_NEW_VALUE_FIELD = 'intValue'
_NEW_DISSOLVED_FIELD = 'dissolved'

layer = iface.activeLayer()

# Create 2 new fields in the layer that will hold the list of neighbors and sum
# of the chosen field.
layer.startEditing()
#layer.dataProvider().addAttributes(
#        [QgsField(_NEW_AREA_FIELD, QVariant.Double),
#         QgsField(_NEW_DISSOLVED_FIELD, QVariant.Int),
#         QgsField(_NEW_VALUE_FIELD, QVariant.Double)])
#layer.updateFields()
#layer.commitChanges()

# Create a dictionary of all features
request=QgsFeatureRequest().addOrderBy(_AREA_FIELD)
feature_dict = {f.id(): f for f in layer.getFeatures(request)}

# Build a spatial index
index = QgsSpatialIndex()

#for f in feature_dict.values():
#    f[_NEW_AREA_FIELD] = f[_AREA_FIELD]
#    f[_NEW_VALUE_FIELD] = f[_VALUE_FIELD]
#    f[_NEW_DISSOLVED_FIELD] = 0
#    layer.updateFeature(f)


for f in feature_dict.values():
    index.insertFeature(f)

#count = 1

geoms = list()
for f in feature_dict.values():
    if(f[_NEW_AREA_FIELD] > 59999999):
        continue
#    if(count > 300):
#        break
#    count = count + 1 
#    print(f[_NEW_AREA_FIELD])
    #print('Working on %s' % tostring(f.id) % 'area:' % tostring(f[_NEW_AREA_FIELD]))
    geom = f.geometry()
    # Find all features that intersect the bounding box of the current feature.
    # We use spatial index to find the features intersecting the bounding box
    # of the current feature. This will narrow down the features that we need
    # to check neighboring features.
    intersecting_ids = index.intersects(geom.boundingBox())
    closest_area = 0
    closest_DN = 0
    closest_id = 0
    #combined = QgsGeometry.fromWkt('GEOMETRYCOLLECTION EMPTY')
    combined = QgsGeometry()
    for intersecting_id in intersecting_ids:
        intersecting_f = feature_dict[intersecting_id]
        if (f != intersecting_f and
            intersecting_f[_NEW_DISSOLVED_FIELD] != 1 and
            not intersecting_f.geometry().disjoint(geom)):
            if (closest_id == 0): 
                closest_DN = intersecting_f[_NEW_VALUE_FIELD]
                closest_area = intersecting_f[_NEW_AREA_FIELD]
                closest_id = intersecting_f.id()
            elif (abs(intersecting_f[_NEW_VALUE_FIELD] - f[_NEW_VALUE_FIELD]) < abs(closest_DN - f[_NEW_VALUE_FIELD])):
                closest_DN = intersecting_f[_NEW_VALUE_FIELD]
                closest_area = intersecting_f[_NEW_AREA_FIELD]
                closest_id = intersecting_f.id()
    for update_f in feature_dict.values():
        if(update_f.id() == closest_id):
            #print(f[_NEW_AREA_FIELD])
            #print(update_f[_NEW_AREA_FIELD])
            #print(update_f.id())
            #print(closest_id)
            sum_area = f[_NEW_AREA_FIELD] + update_f[_NEW_AREA_FIELD]
            sum_value = (f[_NEW_AREA_FIELD]/sum_area * f[_NEW_VALUE_FIELD]) + (update_f[_NEW_AREA_FIELD]/sum_area * update_f[_NEW_VALUE_FIELD])
            update_f[_NEW_AREA_FIELD] = sum_area 
            update_f[_NEW_VALUE_FIELD] = sum_value
            #f[_NEW_DISSOLVED_FIELD] = 1
            #layer.updateFeature(f)
            combined = f.geometry()
            #print(combined)
#            if(combined.isNull()):
#                print("NULL")
#            else:
#                print("NOT NULL")     
#            if(update_f.geometry().isNull()):
#                print("NULL")
#            else:
#                print("NOT NULL")    
            combined = combined.combine(update_f.geometry())
            #print(combined)
            if(combined.isNull()):
                print("NULL")
#                print(combined.Error())
#            else:
#                print("NOT NULL")
                
            #layer.changeGeometry(update_f.id(), combined)
            update_f.setGeometry(combined)
            #layer.updateFeature(update_f)
            layer.deleteFeature(update_f.id())
                        
            layer.deleteFeature(f.id())
            index.deleteFeature(f)
            
            layer.addFeature(update_f)
            
            #index.deleteFeature(update_f)
            #index.addFeature(update_f)
            #layer.commitChanges()
            print(f.id())
            #index = QgsSpatialIndex()   
#            for f in feature_dict.values():
#                index.insertFeature(f)
            break
layer.commitChanges()

    
    

