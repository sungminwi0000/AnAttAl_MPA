# -*- coding: mbcs -*-
# Do not delete the following import lines
from abaqus import *
from abaqusConstants import *
import __main__
# urspüngliche Werte: dB = 1.0 / hB = 16.0 / hE = dE = 10.0 / R = 100 / pB = 1.0 / pE = 2.0 / VE1 = 0.05 / VE2 = 0.2 / VE2min = 0.05 / VE2max = 0.2 / VE3 = 0.5 /
# VE3min = 0.2 / VE3max = 0.5 / VE4 = 1.0 / VE4min = 0.5 / VE4max = 1.0 / VB1 = 8 / VBminmax = 0.05 / VB2 = 4 / VB2min = 0.05 / VB2max = 0.1 / VB3 = 2 / VB3min = 0.1
# VB3max = 0.2 / VB4 = 1 / VB4min = 0.3 / VB4max = 0.5
    
    
def Macro_1_Geometrie():
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import step
    import interaction
    import load
    import mesh
    import optimization
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
        
    # Dicke Blech
    dB = 2.0
    # Länge Blech
    hB = 16.0
    # Höhe Elektrode
    hE = 10.0
    # Position Partition Blech
    pP = 4.0
    pP1 = 1.0 # Entfernung zur Partition pP
    pP2 = 1.0 # Entfernung zur Partition pP1
    # Durchmesser Elektrode
    dE = 20.0
    # Balligkeitsradius Elektrode
    R = 100.0
    # !!! bei Änderung der Blechdicke die Eingaben für Makro 3 beachten !!!
    
    # Part Blech
    s = mdb.models['Model-1'].ConstrainedSketch(name='__profile__', 
        sheetSize=200.0)
    g, v, d, c = s.geometry, s.vertices, s.dimensions, s.constraints
    s.sketchOptions.setValues(viewStyle=AXISYM)
    s.setPrimaryObject(option=STANDALONE)
    s.ConstructionLine(point1=(0.0, -100.0), point2=(0.0, 100.0))
    s.FixedConstraint(entity=g[2])
    session.viewports['Viewport: 1'].view.setValues(nearPlane=82.3794, 
        farPlane=106.182, width=122.993, height=58.3966, cameraPosition=(
        42.6911, 11.3771, 94.2809), cameraTarget=(42.6911, 11.3771, 0))
    s.rectangle(point1=(0.0, 0.0), point2=(hB, -dB))
    p = mdb.models['Model-1'].Part(name='Blech', dimensionality=AXISYMMETRIC, 
        type=DEFORMABLE_BODY)
    p = mdb.models['Model-1'].parts['Blech']
    p.BaseShell(sketch=s)
    s.unsetPrimaryObject()
    p = mdb.models['Model-1'].parts['Blech']
    session.viewports['Viewport: 1'].setValues(displayedObject=p)
    # Part Elektrode oben
    del mdb.models['Model-1'].sketches['__profile__']
    s = mdb.models['Model-1'].ConstrainedSketch(name='__profile__', 
        sheetSize=200.0)
    g, v, d, c = s.geometry, s.vertices, s.dimensions, s.constraints
    s.sketchOptions.setValues(viewStyle=AXISYM)
    s.setPrimaryObject(option=STANDALONE)
    s.ConstructionLine(point1=(0.0, -100.0), point2=(0.0, 100.0))
    s.FixedConstraint(entity=g[2])
    session.viewports['Viewport: 1'].view.setValues(nearPlane=85.6224, 
        farPlane=102.939, width=101.267, height=48.0811, cameraPosition=(
        28.0358, 14.2716, 94.2809), cameraTarget=(28.0358, 14.2716, 0))
    s.Line(point1=(0.0, 0.0), point2=(0.0, hE))
    s.VerticalConstraint(entity=g[3], addUndoState=False)
    s.Line(point1=(0.0, hE), point2=(dE/2, hE))
    s.HorizontalConstraint(entity=g[4], addUndoState=False)
    s.PerpendicularConstraint(entity1=g[3], entity2=g[4], addUndoState=False)
    s.Line(point1=(dE/2, hE), point2=(dE/2, 0.0))
    s.VerticalConstraint(entity=g[5], addUndoState=False)
    s.PerpendicularConstraint(entity1=g[4], entity2=g[5], addUndoState=False)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=91.9197, 
        farPlane=96.6421, width=24.401, height=11.5855, cameraPosition=(
        10.1225, 3.78028, 94.2809), cameraTarget=(10.1225, 3.78028, 0))
    s.ArcByCenterEnds(center=(0.0, R), point1=(0.0, 0.0), point2=(dE/2, 
        0.532939195632935), direction=COUNTERCLOCKWISE)
    s.CoincidentConstraint(entity1=v[4], entity2=g[5], addUndoState=False)
    s.CoincidentConstraint(entity1=v[3], entity2=v[4])
    session.viewports['Viewport: 1'].view.setValues(nearPlane=91.769, 
        farPlane=96.7928, width=31.3811, height=14.8997, cameraPosition=(
        11.3153, 4.42237, 94.2809), cameraTarget=(11.3153, 4.42237, 0))
    p = mdb.models['Model-1'].Part(name='Elektrode_oben', 
        dimensionality=AXISYMMETRIC, type=DEFORMABLE_BODY)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    p.BaseShell(sketch=s)
    s.unsetPrimaryObject()
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    session.viewports['Viewport: 1'].setValues(displayedObject=p)
    del mdb.models['Model-1'].sketches['__profile__']
    session.viewports['Viewport: 1'].view.setValues(nearPlane=28.7218, 
        farPlane=33.7603, width=26.1677, height=12.4244, viewOffsetX=6.17687, 
        viewOffsetY=0.419908)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    f1, e, d1 = p.faces, p.edges, p.datums
    t = p.MakeSketchTransform(sketchPlane=f1[0], sketchPlaneSide=SIDE1, origin=(
        4.957541, 5.082324, 0.0))
    s1 = mdb.models['Model-1'].ConstrainedSketch(name='__profile__', 
        sheetSize=28.28, gridSpacing=0.7, transform=t)
    g, v, d, c = s1.geometry, s1.vertices, s1.dimensions, s1.constraints
    s1.setPrimaryObject(option=SUPERIMPOSE)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    p.projectReferencesOntoSketch(sketch=s1, filter=COPLANAR_EDGES)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=25.508, 
        farPlane=31.0605, width=32.4701, height=15.4167, cameraPosition=(
        9.41743, 6.26682, 28.2843), cameraTarget=(9.41743, 6.26682, 0))
    s1.Line(point1=(5.04245899999997, -3.675), point2=(-4.957541000049, -3.675))
    s1.HorizontalConstraint(entity=g[7], addUndoState=False)
    s1.PerpendicularConstraint(entity1=g[3], entity2=g[7], addUndoState=False)
    s1.CoincidentConstraint(entity1=v[5], entity2=g[3], addUndoState=False)
    s1.CoincidentConstraint(entity1=v[6], entity2=g[5], addUndoState=False)
    s1.Line(point1=(5.04245899999997, -2.275), point2=(-4.957541000049, -2.275))
    s1.HorizontalConstraint(entity=g[8], addUndoState=False)
    s1.PerpendicularConstraint(entity1=g[3], entity2=g[8], addUndoState=False)
    s1.CoincidentConstraint(entity1=v[7], entity2=g[3], addUndoState=False)
    s1.CoincidentConstraint(entity1=v[8], entity2=g[5], addUndoState=False)
    s1.Line(point1=(5.04245899999997, -1.05), point2=(-4.957541000049, -1.05))
    s1.HorizontalConstraint(entity=g[9], addUndoState=False)
    s1.PerpendicularConstraint(entity1=g[3], entity2=g[9], addUndoState=False)
    s1.CoincidentConstraint(entity1=v[9], entity2=g[3], addUndoState=False)
    s1.CoincidentConstraint(entity1=v[10], entity2=g[5], addUndoState=False)
    s1.DistanceDimension(entity1=g[7], entity2=v[0], textPoint=(-5.97985984002686, 
        -4.51209065496826), value=1.75)
    s1.DistanceDimension(entity1=g[7], entity2=g[8], textPoint=(-6.28812075769043, 
        -2.89035889685059), value=1.0)
    s1.DistanceDimension(entity1=g[8], entity2=g[9], textPoint=(-6.24701834832764, 
        -1.8639459329834), value=2.0)
    s1.Line(point1=(0.042459, 4.917676), point2=(0.042459, -0.882323999944598))
    s1.VerticalConstraint(entity=g[10], addUndoState=False)
    s1.PerpendicularConstraint(entity1=g[4], entity2=g[10], addUndoState=False)
    s1.CoincidentConstraint(entity1=v[11], entity2=g[4], addUndoState=False)
    s1.EqualDistanceConstraint(entity1=v[3], entity2=v[4], midpoint=v[11], 
        addUndoState=False)
    s1.CoincidentConstraint(entity1=v[12], entity2=g[9], addUndoState=False)
    s1.EqualDistanceConstraint(entity1=v[9], entity2=v[10], midpoint=v[12], 
        addUndoState=False)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    f = p.faces
    pickedFaces = f.getSequenceFromMask(mask=('[#1 ]', ), )
    e1, d2 = p.edges, p.datums
    p.PartitionFaceBySketch(faces=pickedFaces, sketch=s1)
    s1.unsetPrimaryObject()
    # Part Elektrode unten
    del mdb.models['Model-1'].sketches['__profile__']
    session.viewports['Viewport: 1'].view.setValues(nearPlane=28.3925, 
        farPlane=34.0895, width=33.1319, height=15.731, viewOffsetX=8.74508, 
        viewOffsetY=0.62072)
    p1 = mdb.models['Model-1'].parts['Elektrode_oben']
    session.viewports['Viewport: 1'].setValues(displayedObject=p1)
    p = mdb.models['Model-1'].Part(name='Elektrode_unten', 
        objectToCopy=mdb.models['Model-1'].parts['Elektrode_oben'], 
        compressFeatureList=ON, mirrorPlane=XZPLANE)
    session.viewports['Viewport: 1'].setValues(displayedObject=p)
    p = mdb.models['Model-1'].parts['Blech']
    f, e, d1 = p.faces, p.edges, p.datums
    t = p.MakeSketchTransform(sketchPlane=f[0], sketchPlaneSide=SIDE1, origin=(8.0, 
        -dB/2, 0.0))
    s = mdb.models['Model-1'].ConstrainedSketch(name='__profile__', 
        sheetSize=32.06, gridSpacing=0.8, transform=t)
    g, v, d, c = s.geometry, s.vertices, s.dimensions, s.constraints
    s.setPrimaryObject(option=SUPERIMPOSE)
    # Partition Blech
    p = mdb.models['Model-1'].parts['Blech']
    p.projectReferencesOntoSketch(sketch=s, filter=COPLANAR_EDGES)
    s.Line(point1=(-2.0, 0.5), point2=(-2.0, -0.5))
    s.VerticalConstraint(entity=g[7], addUndoState=False)
    s.PerpendicularConstraint(entity1=g[2], entity2=g[7], addUndoState=False)
    s.CoincidentConstraint(entity1=v[4], entity2=g[2], addUndoState=False)
    s.CoincidentConstraint(entity1=v[5], entity2=g[4], addUndoState=False)
    s.DistanceDimension(entity1=g[3], entity2=g[7], textPoint=(-2.74520540237427, 
        -1.20963716506958), value=pP)
    s.Line(point1=(-1.0, 0.5), point2=(-1.0, -0.5))
    s.VerticalConstraint(entity=g[8], addUndoState=False)
    s.PerpendicularConstraint(entity1=g[2], entity2=g[8], addUndoState=False)
    s.CoincidentConstraint(entity1=v[6], entity2=g[2], addUndoState=False)
    s.CoincidentConstraint(entity1=v[7], entity2=g[4], addUndoState=False)
    s.DistanceDimension(entity1=g[7], entity2=g[8], textPoint=(-1.21095848083496, 
        -0.979751348495483), value=pP1)
    s.Line(point1=(0.4, 0.5), point2=(0.4, -0.5))
    s.VerticalConstraint(entity=g[9], addUndoState=False)
    s.PerpendicularConstraint(entity1=g[2], entity2=g[9], addUndoState=False)
    s.CoincidentConstraint(entity1=v[8], entity2=g[2], addUndoState=False)
    s.CoincidentConstraint(entity1=v[9], entity2=g[4], addUndoState=False)
    s.DistanceDimension(entity1=g[8], entity2=g[9], textPoint=(-0.136985778808594, 
        -0.903122663497925), value=pP2)
    s.Line(point1=(8.0, 0.0), point2=(-2.0, 0.0))
    s.HorizontalConstraint(entity=g[10], addUndoState=False)
    s.PerpendicularConstraint(entity1=g[5], entity2=g[10], addUndoState=False)
    s.CoincidentConstraint(entity1=v[10], entity2=g[5], addUndoState=False)
    s.EqualDistanceConstraint(entity1=v[3], entity2=v[0], midpoint=v[10], 
        addUndoState=False)
    s.CoincidentConstraint(entity1=v[11], entity2=g[7], addUndoState=False)
    s.EqualDistanceConstraint(entity1=v[4], entity2=v[5], midpoint=v[11], 
        addUndoState=False)
    p = mdb.models['Model-1'].parts['Blech']
    f = p.faces
    pickedFaces = f.getSequenceFromMask(mask=('[#1 ]', ), )
    e1, d2 = p.edges, p.datums
    p.PartitionFaceBySketch(faces=pickedFaces, sketch=s)
    s.unsetPrimaryObject()
    del mdb.models['Model-1'].sketches['__profile__']


def Macro_2_Eigenschaften_Al5182():
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import step
    import interaction
    import load
    import mesh
    import optimization
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
    session.viewports['Viewport: 1'].partDisplay.setValues(sectionAssignments=ON, 
        engineeringFeatures=ON)
    session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
        referenceRepresentation=OFF)
    mdb.models['Model-1'].Material(name='CuCrZr')
    mdb.models['Model-1'].materials['CuCrZr'].Elastic(temperatureDependency=ON, 
        table=((135000.0, 0.3, 20.0), ))
    mdb.models['Model-1'].materials['CuCrZr'].Conductivity(
        temperatureDependency=ON, table=((320.0, 20.0), ))
    mdb.models['Model-1'].materials['CuCrZr'].ElectricalConductivity(
        temperatureDependency=ON, table=((43500000.0, 20.0), ))
    mdb.models['Model-1'].materials['CuCrZr'].SpecificHeat(
        temperatureDependency=ON, table=((370000000.0, 20.0), ))
    mdb.models['Model-1'].materials['CuCrZr'].Density(table=((8.91e-09, ), ))
    mdb.models['Model-1'].materials['CuCrZr'].UserOutputVariables(n=6)
    mdb.models['Model-1'].Material(name='Al5182')
    mdb.models['Model-1'].materials['Al5182'].Conductivity(
        temperatureDependency=ON, table=((123.5, 20.0), (132.3, 50.0), (141.1, 
        100.0), (149.7, 150.0), (158.4, 200.0), (167.1, 250.0), (175.8, 300.0), 
        (184.6, 350.0), (193.3, 400.0), (202.0, 450.0), (210.7, 500.0)))
    mdb.models['Model-1'].materials['Al5182'].Density(temperatureDependency=ON, 
        table=((2.66e-09, 20.0), (2.65e-09, 50.0), (2.64e-09, 100.0), (
        2.63e-09, 150.0), (2.62e-09, 200.0), (2.61e-09, 250.0), (2.6e-09, 
        300.0), (2.59e-09, 350.0), (2.58e-09, 400.0), (2.56e-09, 450.0), (
        2.55e-09, 500.0)))
    mdb.models['Model-1'].materials['Al5182'].LatentHeat(table=((372000000000.0, 
        568.0, 635.0), ))
    mdb.models['Model-1'].materials['Al5182'].SpecificHeat(
        temperatureDependency=ON, table=((904900000.0, 20.0), (917200000.0, 
        50.0), (929500000.0, 100.0), (941700000.0, 150.0), (954000000.0, 
        200.0), (966300000.0, 250.0), (978600000.0, 300.0), (990800000.0, 
        350.0), (1003100000.0, 400.0), (1015400000.0, 450.0), (1027700000.0, 
        500.0)))
    mdb.models['Model-1'].materials['Al5182'].ElectricalConductivity(
        temperatureDependency=ON, table=((16920830.0, 30.0), (15970830.0, 
        100.0), (14973220.0, 150.0), (14045810.0, 200.0), (13033650.0, 250.0), 
        (12102670.0, 300.0), (11229920.0, 350.0), (10454150.0, 400.0), (
        9707811.0, 450.0), (9080667.0, 500.0)))
    mdb.models['Model-1'].materials['Al5182'].Elastic(temperatureDependency=ON, 
        table=((71300.0, 0.33, 20.0), (70300.0, 0.33, 50.0), (68900.0, 0.33, 
        100.0), (67200.0, 0.33, 150.0), (65100.0, 0.33, 200.0), (62700.0, 0.33, 
        250.0), (59800.0, 0.33, 300.0), (56600.0, 0.33, 350.0), (53000.0, 0.33, 
        400.0), (44950.0, 0.33, 500.0)))
    mdb.models['Model-1'].materials['Al5182'].Expansion(table=((2.306e-05, 20.0), (
        2.338e-05, 50.0), (2.392e-05, 100.0), (2.446e-05, 150.0), (2.5e-05, 
        200.0), (2.554e-05, 250.0), (2.608e-05, 300.0), (2.662e-05, 350.0), (
        2.715e-05, 400.0), (2.769e-05, 450.0), (2.823e-05, 500.0)), 
        temperatureDependency=ON)
    mdb.models['Model-1'].materials['Al5182'].Plastic(temperatureDependency=ON, 
        table=((36.3, 0.0, 20.0), (50.06, 1.6e-05, 20.0), (100.25, 2.6e-05, 
        20.0), (151.6, 0.00854, 20.0), (207.86, 0.035685, 20.0), (272.86, 
        0.081362, 20.0), (322.07, 0.153103, 20.0), (341.1, 0.192599, 20.0), (
        133.1, 0.0, 100.0), (151.6, 0.00854, 100.0), (207.68, 0.035685, 100.0), 
        (238.69, 0.05616, 100.0), (279.38, 0.105555, 100.0), (316.43, 0.170775, 
        100.0), (27.15, 0.0, 200.0), (51.04, 0.000545, 200.0), (100.59, 
        0.001017, 200.0), (152.98, 0.011678, 200.0), (180.57, 0.029146, 200.0), 
        (215.16, 0.068818, 200.0), (266.8, 0.192787, 200.0), (81.21, 0.0, 
        300.0), (100.68, 0.002842, 300.0), (116.67, 0.008662, 300.0), (127.7, 
        0.019094, 300.0), (137.31, 0.03222, 300.0), (154.84, 0.096814, 300.0), 
        (162.88, 0.182983, 300.0), (23.98, 0.0, 400.0), (39.48, 0.000785, 
        400.0), (49.16, 0.00134, 400.0), (61.22, 0.002978, 400.0), (69.4, 
        0.005081, 400.0), (81.44, 0.009027, 400.0), (89.9, 0.025933, 400.0), (
        89.17, 0.163433, 400.0), (26.7, 0.0, 500.0), (30.9, 0.000536, 500.0), (
        35.34, 0.001283, 500.0), (40.15, 0.007554, 500.0), (41.99, 0.043323, 
        500.0), (41.71, 0.095025, 500.0), (41.64, 0.141454, 500.0), (5.0, 0.0, 
        600.0), (5.0, 5.0, 600.0)))
    mdb.models['Model-1'].materials['Al5182'].plastic.AnnealTemperature(table=((
        600.0, ), ))        
    mdb.models['Model-1'].materials['Al5182'].UserDefinedField()
    mdb.models['Model-1'].materials['Al5182'].UserOutputVariables(n=6)
    mdb.models['Model-1'].materials['Al5182'].Depvar(n=10)
    mdb.models['Model-1'].HomogeneousSolidSection(name='Section-1', 
        material='CuCrZr', thickness=1.0)
    mdb.models['Model-1'].HomogeneousSolidSection(name='Section-2', 
        material='Al5182', thickness=1.0)
    mdb.models['Model-1'].HomogeneousSolidSection(name='Section-3', 
        material='CuCrZr', thickness=1.0)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    session.viewports['Viewport: 1'].setValues(displayedObject=p)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=28.2053, 
        farPlane=34.2767, width=13.8052, height=16.557, viewOffsetX=0.761978, 
        viewOffsetY=-0.335184)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    f = p.faces
    faces = f.getSequenceFromMask(mask=('[#1f ]', ), )
    region = regionToolset.Region(faces=faces)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    p.SectionAssignment(region=region, sectionName='Section-1', offset=0.0, 
        offsetType=MIDDLE_SURFACE, offsetField='', 
        thicknessAssignment=FROM_SECTION)
    p = mdb.models['Model-1'].parts['Blech']
    session.viewports['Viewport: 1'].setValues(displayedObject=p)
    p = mdb.models['Model-1'].parts['Blech']
    f = p.faces
    faces = f.getSequenceFromMask(mask=('[#7f ]', ), )
    region = regionToolset.Region(faces=faces)
    p = mdb.models['Model-1'].parts['Blech']
    p.SectionAssignment(region=region, sectionName='Section-2', offset=0.0, 
        offsetType=MIDDLE_SURFACE, offsetField='', 
        thicknessAssignment=FROM_SECTION)
    p = mdb.models['Model-1'].parts['Elektrode_unten']
    session.viewports['Viewport: 1'].setValues(displayedObject=p)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=26.3659, 
        farPlane=36.1161, width=19.9817, height=23.9647, viewOffsetX=-1.00126, 
        viewOffsetY=1.43694)
    p = mdb.models['Model-1'].parts['Elektrode_unten']
    f = p.faces
    faces = f.getSequenceFromMask(mask=('[#1f ]', ), )
    region = regionToolset.Region(faces=faces)
    p = mdb.models['Model-1'].parts['Elektrode_unten']
    p.SectionAssignment(region=region, sectionName='Section-3', offset=0.0, 
        offsetType=MIDDLE_SURFACE, offsetField='', 
        thicknessAssignment=FROM_SECTION)


def Macro_3_Zusammenbau():
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import step
    import interaction
    import load
    import mesh
    import optimization
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
    
    dB = 2.0
    # Position Blech unten, Differenz der Blechdicke addieren
    # pB = 1.0
    pB = dB
    # Position Elektrode unten, 2-fach Differenz der Blechdicke addieren
    # pE = 2.0
    pE = 2*dB
    
    a = mdb.models['Model-1'].rootAssembly
    session.viewports['Viewport: 1'].setValues(displayedObject=a)
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(
        optimizationTasks=OFF, geometricRestrictions=OFF, stopConditions=OFF)
    a = mdb.models['Model-1'].rootAssembly
    a.DatumCsysByThreePoints(coordSysType=CYLINDRICAL, origin=(0.0, 0.0, 0.0), 
        point1=(1.0, 0.0, 0.0), point2=(0.0, 0.0, -1.0))
    p = mdb.models['Model-1'].parts['Blech']
    a.Instance(name='Blech-1', part=p, dependent=ON)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    a.Instance(name='Elektrode_oben', part=p, dependent=ON)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=38.5037, 
        farPlane=44.4652, width=34.7025, height=16.4767, viewOffsetX=2.73041, 
        viewOffsetY=-1.08953)
    a = mdb.models['Model-1'].rootAssembly
    p = mdb.models['Model-1'].parts['Blech']
    a.Instance(name='Blech-2', part=p, dependent=ON)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=38.5037, 
        farPlane=44.4652, width=34.7025, height=16.4767, viewOffsetX=3.46719, 
        viewOffsetY=-0.146714)
    a = mdb.models['Model-1'].rootAssembly
    a.translate(instanceList=('Blech-2', ), vector=(0.0, -pB, 0.0))
    session.viewports['Viewport: 1'].view.setValues(nearPlane=38.3315, 
        farPlane=44.6374, width=32.7407, height=15.5452, viewOffsetX=5.89743, 
        viewOffsetY=-3.09594)
    a = mdb.models['Model-1'].rootAssembly
    p = mdb.models['Model-1'].parts['Elektrode_unten']
    a.Instance(name='Elektrode_unten', part=p, dependent=ON)
    a = mdb.models['Model-1'].rootAssembly
    a.translate(instanceList=('Elektrode_unten', ), vector=(0.0, -pE, 0.0))


def Macro_4_Oberflaechen():
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import step
    import interaction
    import load
    import mesh
    import optimization
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
    session.viewports['Viewport: 1'].view.setValues(nearPlane=57.5002, 
        farPlane=65.98, width=49.356, height=23.4341, viewOffsetX=11.6915, 
        viewOffsetY=0.388816)
    a = mdb.models['Model-1'].rootAssembly
    s1 = a.instances['Elektrode_oben'].edges
    side1Edges1 = s1.getSequenceFromMask(mask=('[#44 ]', ), )
    a.Surface(side1Edges=side1Edges1, name='SURF_LOAD_CURRENT')
    a = mdb.models['Model-1'].rootAssembly
    s1 = a.instances['Elektrode_oben'].edges
    side1Edges1 = s1.getSequenceFromMask(mask=('[#200 ]', ), )
    a.Surface(side1Edges=side1Edges1, name='SURF_EL1_BL1')
    a = mdb.models['Model-1'].rootAssembly
    s1 = a.instances['Elektrode_oben'].edges
    side1Edges1 = s1.getSequenceFromMask(mask=('[#200 ]', ), )
    a.Surface(side1Edges=side1Edges1, name='SURF2_EL1_BL1')
    a = mdb.models['Model-1'].rootAssembly
    s1 = a.instances['Elektrode_unten'].edges
    side1Edges1 = s1.getSequenceFromMask(mask=('[#200 ]', ), )
    a.Surface(side1Edges=side1Edges1, name='SURF_EL2_BL2')
    a = mdb.models['Model-1'].rootAssembly
    s1 = a.instances['Elektrode_unten'].edges
    side1Edges1 = s1.getSequenceFromMask(mask=('[#200 ]', ), )
    a.Surface(side1Edges=side1Edges1, name='SURF2_EL2_BL2')
    a = mdb.models['Model-1'].rootAssembly
    s1 = a.instances['Blech-1'].edges
    side1Edges1 = s1.getSequenceFromMask(mask=('[#30840 ]', ), )
    a.Surface(side1Edges=side1Edges1, name='SURF_BL1_EL1')
    a = mdb.models['Model-1'].rootAssembly
    s1 = a.instances['Blech-1'].edges
    side1Edges1 = s1.getSequenceFromMask(mask=('[#30840 ]', ), )
    a.Surface(side1Edges=side1Edges1, name='SURF2_BL1_EL1')
    a = mdb.models['Model-1'].rootAssembly
    s1 = a.instances['Blech-2'].edges
    side1Edges1 = s1.getSequenceFromMask(mask=('[#82102 ]', ), )
    a.Surface(side1Edges=side1Edges1, name='SURF_BL2_EL2')
    a = mdb.models['Model-1'].rootAssembly
    s1 = a.instances['Blech-2'].edges
    side1Edges1 = s1.getSequenceFromMask(mask=('[#82102 ]', ), )
    a.Surface(side1Edges=side1Edges1, name='SURF2_BL2_EL2')
    session.viewports['Viewport: 1'].assemblyDisplay.hideInstances(instances=(
        'Blech-2', ))
    a = mdb.models['Model-1'].rootAssembly
    s1 = a.instances['Blech-1'].edges
    side1Edges1 = s1.getSequenceFromMask(mask=('[#82102 ]', ), )
    a.Surface(side1Edges=side1Edges1, name='SURF_BL1_BL2')
    a = mdb.models['Model-1'].rootAssembly
    s1 = a.instances['Blech-1'].edges
    side1Edges1 = s1.getSequenceFromMask(mask=('[#82102 ]', ), )
    a.Surface(side1Edges=side1Edges1, name='SURF2_BL1_BL2')
    session.viewports['Viewport: 1'].assemblyDisplay.hideInstances(instances=(
        'Blech-1', ))
    session.viewports['Viewport: 1'].assemblyDisplay.showInstances(instances=(
        'Blech-2', ))
    a = mdb.models['Model-1'].rootAssembly
    s1 = a.instances['Blech-2'].edges
    side1Edges1 = s1.getSequenceFromMask(mask=('[#30840 ]', ), )
    a.Surface(side1Edges=side1Edges1, name='SURF_BL2_BL1')
    a = mdb.models['Model-1'].rootAssembly
    s1 = a.instances['Blech-2'].edges
    side1Edges1 = s1.getSequenceFromMask(mask=('[#30840 ]', ), )
    a.Surface(side1Edges=side1Edges1, name='SURF2_BL2_BL1')
    session.viewports['Viewport: 1'].assemblyDisplay.showInstances(instances=(
        'Blech-1', ))


def Macro_5_Schritte():
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import step
    import interaction
    import load
    import mesh
    import optimization
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
    # Anlegen der Steps
    mdb.models['Model-1'].StaticStep(name='Step-1', previous='Initial')
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-1')
    mdb.models['Model-1'].StaticStep(name='Step-2', previous='Step-1')
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-2')
    mdb.models['Model-1'].StaticStep(name='Step-3', previous='Step-2')
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-3')
    # Field Output der Steps
    mdb.models['Model-1'].steps['Step-1'].setValues(nlgeom=ON)
    mdb.models['Model-1'].fieldOutputRequests['F-Output-1'].deactivate('Step-2')
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-1')
    mdb.models['Model-1'].fieldOutputRequests['F-Output-1'].setValues(variables=(
        'CDISP', 'CF', 'CSTRESS', 'LE', 'PE', 'PEEQ', 'PEMAG', 'RF', 'S', 'U'))
    mdb.models['Model-1'].FieldOutputRequest(name='F-Output-2', 
        createStepName='Step-1', variables=('SDV', 'FV'))
    mdb.models['Model-1'].FieldOutputRequest(name='F-Output-3', 
        createStepName='Step-1', variables=('COORD', ))
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-2')
    mdb.models['Model-1'].FieldOutputRequest(name='F-Output-4', 
        createStepName='Step-2', variables=('CDISP', 'CF', 'CSTRESS', 'LE', 
        'PE', 'PEEQ', 'PEMAG', 'RF', 'S', 'U'))
    mdb.models['Model-1'].FieldOutputRequest(name='F-Output-5', 
        createStepName='Step-2', variables=('SDV', 'FV'))
    mdb.models['Model-1'].FieldOutputRequest(name='F-Output-6', 
        createStepName='Step-2', variables=('COORD', ))
    mdb.models['Model-1'].FieldOutputRequest(name='F-Output-7', 
        createStepName='Step-2', variables=('BDSTAT', 'CDISP', 'CDISPETOS', 
        'CDSTRESS', 'CFORCE', 'CNAREA', 'CRSTS', 'CSDMG', 'CSMAXSCRT', 
        'CSMAXUCRT', 'CSQUADSCRT', 'CSQUADUCRT', 'CSTATUS', 'CSTRESS', 
        'CSTRESSERI', 'CSTRESSETOS', 'DBS', 'DBSF', 'DBT', 'ECD', 'ECDA', 
        'ECDT', 'ECDTA', 'EFENRRTR', 'ENRRT', 'OPENBC', 'PPRESS', 'SJD', 
        'SJDA', 'SJDT', 'SJDTA'))
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-3')
    mdb.models['Model-1'].FieldOutputRequest(name='F-Output-8', 
        createStepName='Step-3', variables=('CDISP', 'CF', 'CSTRESS', 'LE', 
        'PE', 'PEEQ', 'PEMAG', 'RF', 'S', 'U'))
    mdb.models['Model-1'].FieldOutputRequest(name='F-Output-9', 
        createStepName='Step-3', variables=('SDV', 'FV'))
    mdb.models['Model-1'].FieldOutputRequest(name='F-Output-10', 
        createStepName='Step-3', variables=('BDSTAT', 'CDISP', 'CDISPETOS', 
        'CDSTRESS', 'CFORCE', 'CNAREA', 'CRSTS', 'CSDMG', 'CSMAXSCRT', 
        'CSMAXUCRT', 'CSQUADSCRT', 'CSQUADUCRT', 'CSTATUS', 'CSTRESS', 
        'CSTRESSERI', 'CSTRESSETOS', 'DBS', 'DBSF', 'DBT', 'ECD', 'ECDA', 
        'ECDT', 'ECDTA', 'EFENRRTR', 'ENRRT', 'OPENBC', 'PPRESS', 'SJD', 
        'SJDA', 'SJDT', 'SJDTA'))
    mdb.models['Model-1'].FieldOutputRequest(name='F-Output-11', 
        createStepName='Step-3', variables=('COORD', ))
    mdb.models['Model-1'].fieldOutputRequests['F-Output-2'].deactivate('Step-2')
    mdb.models['Model-1'].fieldOutputRequests['F-Output-3'].deactivate('Step-2')
    mdb.models['Model-1'].fieldOutputRequests['F-Output-4'].deactivate('Step-3')
    mdb.models['Model-1'].fieldOutputRequests['F-Output-5'].deactivate('Step-3')
    mdb.models['Model-1'].fieldOutputRequests['F-Output-6'].deactivate('Step-3')
    mdb.models['Model-1'].fieldOutputRequests['F-Output-7'].deactivate('Step-3')
    # History Output der Steps
    mdb.models['Model-1'].historyOutputRequests['H-Output-1'].deactivate('Step-2')
    mdb.models['Model-1'].historyOutputRequests['H-Output-1'].setValues(
        variables=PRESELECT)
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-1')
    mdb.models['Model-1'].HistoryOutputRequest(name='H-Output-2', 
        createStepName='Step-1', variables=('CAREA', ))
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-2')
    mdb.models['Model-1'].HistoryOutputRequest(name='H-Output-3', 
        createStepName='Step-2', variables=PRESELECT)
    mdb.models['Model-1'].HistoryOutputRequest(name='H-Output-4', 
        createStepName='Step-2', variables=('CAREA', ))
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-3')
    mdb.models['Model-1'].HistoryOutputRequest(name='H-Output-5', 
        createStepName='Step-3', variables=PRESELECT)
    mdb.models['Model-1'].HistoryOutputRequest(name='H-Output-6', 
        createStepName='Step-3', variables=('CAREA', ))
    mdb.models['Model-1'].historyOutputRequests['H-Output-2'].deactivate('Step-2')
    mdb.models['Model-1'].historyOutputRequests['H-Output-3'].deactivate('Step-3')
    mdb.models['Model-1'].historyOutputRequests['H-Output-4'].deactivate('Step-3')


def Macro_6_Vernetzung():
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import step
    import interaction
    import load
    import mesh
    import optimization
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
    
    # Vernetzung Elektrode
    # Vernetzung beginnend bei Radius der Elektrode
    VE1 = 0.05
    # Vernetzung 1. Partition
    VE2 = 0.2
    VE2min = 0.05
    VE2max = 0.2
    # Vernetzung 2. Partition
    VE3 = 0.5
    VE3min = 0.2
    VE3max = 0.5
    # Vernetzung 3. Patition
    VE4 = 1.0 # nicht verändern!
    VE4min = 0.5 # nicht verändern!
    VE4max = 1.0 # nicht verändern!
    # Vernetzung Blech
    # Vernetzung 1. Partition
    VB1 = 16
    VB1minmax = 0.05
    # Vernetzung 2. Partition
    VB2 = 8
    VB2min = 0.05
    VB2max = 0.1
    # Vernetzung 3. Partition
    VB3 = 4
    VB3min = 0.1
    VB3max = 0.2
    # Vernetzung 4. Partition
    VB4 = 2
    VB4min = 0.3
    VB4max = 0.5 
    # Vernetzung 5.Partition
    VB5 = 1
    
    session.viewports['Viewport: 1'].view.setValues(nearPlane=56.2997, 
        farPlane=67.1805, width=56.394, height=26.8437, viewOffsetX=12.8054, 
        viewOffsetY=0.749835)
    a = mdb.models['Model-1'].rootAssembly
    a.makeIndependent(instances=(a.instances['Blech-1'], a.instances['Blech-2'], ))
    a = mdb.models['Model-1'].rootAssembly
    a.makeIndependent(instances=(a.instances['Elektrode_oben'], ))
    a = mdb.models['Model-1'].rootAssembly
    a.makeIndependent(instances=(a.instances['Elektrode_unten'], ))
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_oben'].edges
    # Vernetzung Elektroden
    e2 = a.instances['Elektrode_unten'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#780 ]', ), )+\
        e2.getSequenceFromMask(mask=('[#780 ]', ), )
    a.seedEdgeBySize(edges=pickedEdges, size=VE1, deviationFactor=0.1, 
        constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_oben'].edges
    e2 = a.instances['Elektrode_unten'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#800 ]', ), )+\
        e2.getSequenceFromMask(mask=('[#800 ]', ), )
    a.seedEdgeBySize(edges=pickedEdges, size=VE2, deviationFactor=0.1, 
        constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_oben'].edges
    e2 = a.instances['Elektrode_unten'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#11 ]', ), )+\
        e2.getSequenceFromMask(mask=('[#11 ]', ), )
    a.seedEdgeBySize(edges=pickedEdges, size=VE3, deviationFactor=0.1, 
        constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_oben'].edges
    e2 = a.instances['Elektrode_unten'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#44 ]', ), )+\
        e2.getSequenceFromMask(mask=('[#24 ]', ), )
    a.seedEdgeBySize(edges=pickedEdges, size=VE4, deviationFactor=0.1, 
        constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_oben'].edges
    pickedEdges1 = e1.getSequenceFromMask(mask=('[#4000 ]', ), )
    pickedEdges2 = e1.getSequenceFromMask(mask=('[#8000 ]', ), )
    a.seedEdgeByBias(biasMethod=SINGLE, end1Edges=pickedEdges1, 
        end2Edges=pickedEdges2, minSize=VE2min, maxSize=VE2max, constraint=FINER)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=57.9778, 
        farPlane=65.5024, width=43.7288, height=20.815, viewOffsetX=7.65041, 
        viewOffsetY=0.911122)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_oben'].edges
    pickedEdges1 = e1.getSequenceFromMask(mask=('[#2000 ]', ), )
    pickedEdges2 = e1.getSequenceFromMask(mask=('[#1000 ]', ), )
    a.seedEdgeByBias(biasMethod=SINGLE, end1Edges=pickedEdges1, 
        end2Edges=pickedEdges2, minSize=VE3min, maxSize=VE3max, constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_oben'].edges
    pickedEdges1 = e1.getSequenceFromMask(mask=('[#8 ]', ), )
    pickedEdges2 = e1.getSequenceFromMask(mask=('[#22 ]', ), )
    a.seedEdgeByBias(biasMethod=SINGLE, end1Edges=pickedEdges1, 
        end2Edges=pickedEdges2, minSize=VE4min, maxSize=VE4max, constraint=FINER)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=57.2632, 
        farPlane=66.217, width=51.9994, height=24.7518, viewOffsetX=11.3125, 
        viewOffsetY=-0.286644)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_unten'].edges
    pickedEdges1 = e1.getSequenceFromMask(mask=('[#8000 ]', ), )
    pickedEdges2 = e1.getSequenceFromMask(mask=('[#4000 ]', ), )
    a.seedEdgeByBias(biasMethod=SINGLE, end1Edges=pickedEdges1, 
        end2Edges=pickedEdges2, minSize=VE2min, maxSize=VE2max, constraint=FINER)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=58.0555, 
        farPlane=65.4247, width=42.8297, height=20.3871, viewOffsetX=8.48669, 
        viewOffsetY=-0.889156)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_unten'].edges
    pickedEdges1 = e1.getSequenceFromMask(mask=('[#1000 ]', ), )
    pickedEdges2 = e1.getSequenceFromMask(mask=('[#2000 ]', ), )
    a.seedEdgeByBias(biasMethod=SINGLE, end1Edges=pickedEdges1, 
        end2Edges=pickedEdges2, minSize=VE3min, maxSize=VE3max, constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_unten'].edges
    pickedEdges1 = e1.getSequenceFromMask(mask=('[#2 ]', ), )
    pickedEdges2 = e1.getSequenceFromMask(mask=('[#48 ]', ), )
    a.seedEdgeByBias(biasMethod=SINGLE, end1Edges=pickedEdges1, 
        end2Edges=pickedEdges2, minSize=VE4min, maxSize=VE4max, constraint=FINER)
    # Vernetzung Blech 1
    session.viewports['Viewport: 1'].assemblyDisplay.hideInstances(instances=(
        'Blech-2', ))
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-1'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#e9000 ]', ), )
    a.seedEdgeBySize(edges=pickedEdges, size=VB1minmax, deviationFactor=0.1, 
        constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-1'].edges
    pickedEdges1 = e1.getSequenceFromMask(mask=('[#2000 ]', ), )
    pickedEdges2 = e1.getSequenceFromMask(mask=('[#14000 ]', ), )
    a.seedEdgeByBias(biasMethod=SINGLE, end1Edges=pickedEdges1, 
        end2Edges=pickedEdges2, minSize=VB2min, maxSize=VB2max, constraint=FINER)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=60.869, 
        farPlane=62.6112, width=8.98715, height=4.27791, viewOffsetX=2.47874, 
        viewOffsetY=0.391745)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-1'].edges
    pickedEdges1 = e1.getSequenceFromMask(mask=('[#100 ]', ), )
    pickedEdges2 = e1.getSequenceFromMask(mask=('[#a00 ]', ), )
    a.seedEdgeByBias(biasMethod=SINGLE, end1Edges=pickedEdges1, 
        end2Edges=pickedEdges2, minSize=VB3min, maxSize=VB3max, constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-1'].edges
    pickedEdges1 = e1.getSequenceFromMask(mask=('[#2 ]', ), )
    pickedEdges2 = e1.getSequenceFromMask(mask=('[#48 ]', ), )
    a.seedEdgeByBias(biasMethod=SINGLE, end1Edges=pickedEdges1, 
        end2Edges=pickedEdges2, minSize=VB4min, maxSize=VB4max, constraint=FINER)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=60.6882, 
        farPlane=62.792, width=12.2592, height=5.83543, viewOffsetX=3.25002, 
        viewOffsetY=0.268266)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-1'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#480 ]', ), )
    a.seedEdgeBySize(edges=pickedEdges, size=0.1, deviationFactor=0.1, 
        constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-1'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#9000 ]', ), )
    a.seedEdgeByNumber(edges=pickedEdges, number=VB2, constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-1'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#480 ]', ), )
    a.seedEdgeByNumber(edges=pickedEdges, number=VB3, constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-1'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#11 ]', ), )
    a.seedEdgeByNumber(edges=pickedEdges, number=VB4, constraint=FINER)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=60.5521, 
        farPlane=62.9281, width=13.8431, height=6.58935, viewOffsetX=3.71714, 
        viewOffsetY=0.224259)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-1'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#24 ]', ), )
    a.seedEdgeByNumber(edges=pickedEdges, number=VB5, constraint=FINER)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=60.795, 
        farPlane=62.6852, width=9.75173, height=4.64185, viewOffsetX=-4.96673, 
        viewOffsetY=0.667577)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-1'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#40000 ]', ), )
    a.seedEdgeByNumber(edges=pickedEdges, number=VB1, constraint=FINER)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=59.8395, 
        farPlane=63.6407, width=22.1315, height=10.5346, viewOffsetX=0.304262, 
        viewOffsetY=0.994693)
    session.viewports['Viewport: 1'].assemblyDisplay.hideInstances(instances=(
        'Blech-1', ))
    # Vernetzung Blech unten
    session.viewports['Viewport: 1'].assemblyDisplay.showInstances(instances=(
        'Blech-2', ))
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-2'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#40000 ]', ), )
    a.seedEdgeByNumber(edges=pickedEdges, number=VB1, constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-2'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#9000 ]', ), )
    a.seedEdgeByNumber(edges=pickedEdges, number=VB2, constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-2'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#480 ]', ), )
    a.seedEdgeByNumber(edges=pickedEdges, number=VB3, constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-2'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#11 ]', ), )
    a.seedEdgeByNumber(edges=pickedEdges, number=VB4, constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-2'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#24 ]', ), )
    a.seedEdgeByNumber(edges=pickedEdges, number=VB5, constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-2'].edges
    pickedEdges = e1.getSequenceFromMask(mask=('[#a0000 ]', ), )
    a.seedEdgeBySize(edges=pickedEdges, size=VB1minmax, deviationFactor=0.1, 
        constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-2'].edges
    pickedEdges1 = e1.getSequenceFromMask(mask=('[#2000 ]', ), )
    pickedEdges2 = e1.getSequenceFromMask(mask=('[#14000 ]', ), )
    a.seedEdgeByBias(biasMethod=SINGLE, end1Edges=pickedEdges1, 
        end2Edges=pickedEdges2, minSize=VB2min, maxSize=VB2max, constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-2'].edges
    pickedEdges1 = e1.getSequenceFromMask(mask=('[#100 ]', ), )
    pickedEdges2 = e1.getSequenceFromMask(mask=('[#a00 ]', ), )
    a.seedEdgeByBias(biasMethod=SINGLE, end1Edges=pickedEdges1, 
        end2Edges=pickedEdges2, minSize=VB3min, maxSize=VB3max, constraint=FINER)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-2'].edges
    pickedEdges1 = e1.getSequenceFromMask(mask=('[#2 ]', ), )
    pickedEdges2 = e1.getSequenceFromMask(mask=('[#48 ]', ), )
    a.seedEdgeByBias(biasMethod=SINGLE, end1Edges=pickedEdges1, 
        end2Edges=pickedEdges2, minSize=VB4min, maxSize=VB4max, constraint=FINER)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=58.9995, 
        farPlane=64.4807, width=31.8897, height=15.1796, viewOffsetX=3.64775, 
        viewOffsetY=1.13071)
    session.viewports['Viewport: 1'].assemblyDisplay.showInstances(instances=(
        'Blech-1', ))
    session.viewports['Viewport: 1'].view.setValues(nearPlane=57.3279, 
        farPlane=66.1523, width=54.744, height=26.0583, viewOffsetX=9.62315, 
        viewOffsetY=1.58908)
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Elektrode_oben'].faces
    pickedRegions = f1.getSequenceFromMask(mask=('[#10 ]', ), )
    a.setMeshControls(regions=pickedRegions, elemShape=QUAD, technique=STRUCTURED)
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Elektrode_unten'].faces
    pickedRegions = f1.getSequenceFromMask(mask=('[#10 ]', ), )
    a.setMeshControls(regions=pickedRegions, elemShape=QUAD, technique=STRUCTURED)
    a = mdb.models['Model-1'].rootAssembly
    partInstances =(a.instances['Blech-1'], a.instances['Elektrode_oben'], 
        a.instances['Blech-2'], a.instances['Elektrode_unten'], )
    a.generateMesh(regions=partInstances)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=27.7011, 
        farPlane=36.4786, width=47.7503, height=21.723, viewOffsetX=11.3523, 
        viewOffsetY=-1.27167)
    mdb.models['Model-1'].setValues(noPartsInputFile=ON)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=27.3323, 
        farPlane=36.8475, width=53.3211, height=26.1612, viewOffsetX=11.7715, 
        viewOffsetY=-1.62575)
    # Änderung von CAX4R auf CAX4
    elemType1 = mesh.ElemType(elemCode=CAX4, elemLibrary=STANDARD)
    elemType2 = mesh.ElemType(elemCode=CAX3, elemLibrary=STANDARD)
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Elektrode_oben'].faces
    faces1 = f1.getSequenceFromMask(mask=('[#1f ]', ), )
    f2 = a.instances['Blech-1'].faces
    faces2 = f2.getSequenceFromMask(mask=('[#7f ]', ), )
    f3 = a.instances['Blech-2'].faces
    faces3 = f3.getSequenceFromMask(mask=('[#7f ]', ), )
    f4 = a.instances['Elektrode_unten'].faces
    faces4 = f4.getSequenceFromMask(mask=('[#1f ]', ), )
    pickedRegions =((faces1+faces2+faces3+faces4), )
    a.setElementType(regions=pickedRegions, elemTypes=(elemType1, elemType2))
    
    
def Macro_7_Sets():
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import step
    import interaction
    import load
    import mesh
    import optimization
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
    session.viewports['Viewport: 1'].view.setValues(nearPlane=57.3303, 
        farPlane=66.1499, width=22.5273, height=24.3043, viewOffsetX=-3.26863, 
        viewOffsetY=-0.365616)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_unten'].edges
    edges1 = e1.getSequenceFromMask(mask=('[#24 ]', ), )
    a.Set(edges=edges1, name='BC_ELEKTRODE')
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-2'].edges
    edges1 = e1.getSequenceFromMask(mask=('[#24 ]', ), )
    e2 = a.instances['Blech-1'].edges
    edges2 = e2.getSequenceFromMask(mask=('[#24 ]', ), )
    a.Set(edges=edges1+edges2, name='BC_TEMP')
    session.viewports['Viewport: 1'].assemblyDisplay.hideInstances(instances=(
        'Blech-1', ))
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Blech-2'].faces
    faces1 = f1.getSequenceFromMask(mask=('[#7f ]', ), )
    a.Set(faces=faces1, name='BLECH2')
    session.viewports['Viewport: 1'].assemblyDisplay.hideInstances(instances=(
        'Blech-2', ))
    session.viewports['Viewport: 1'].assemblyDisplay.showInstances(instances=(
        'Blech-1', ))
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Blech-1'].faces
    faces1 = f1.getSequenceFromMask(mask=('[#7f ]', ), )
    a.Set(faces=faces1, name='BLECH1')
    session.viewports['Viewport: 1'].assemblyDisplay.showInstances(instances=(
        'Blech-2', ))
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Blech-1'].faces
    faces1 = f1.getSequenceFromMask(mask=('[#7f ]', ), )
    f2 = a.instances['Elektrode_oben'].faces
    faces2 = f2.getSequenceFromMask(mask=('[#1f ]', ), )
    f3 = a.instances['Blech-2'].faces
    faces3 = f3.getSequenceFromMask(mask=('[#7f ]', ), )
    f4 = a.instances['Elektrode_unten'].faces
    faces4 = f4.getSequenceFromMask(mask=('[#1f ]', ), )
    a.Set(faces=faces1+faces2+faces3+faces4, name='EALL')
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Elektrode_oben'].faces
    faces1 = f1.getSequenceFromMask(mask=('[#1f ]', ), )
    a.Set(faces=faces1, name='ELEKTRODE1')
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Elektrode_unten'].faces
    faces1 = f1.getSequenceFromMask(mask=('[#1f ]', ), )
    a.Set(faces=faces1, name='ELEKTRODE2')
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_oben'].edges
    edges1 = e1.getSequenceFromMask(mask=('[#44 ]', ), )
    a.Set(edges=edges1, name='LOAD_CURRENT')
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Blech-1'].faces
    faces1 = f1.getSequenceFromMask(mask=('[#7f ]', ), )
    f2 = a.instances['Elektrode_oben'].faces
    faces2 = f2.getSequenceFromMask(mask=('[#1f ]', ), )
    f3 = a.instances['Blech-2'].faces
    faces3 = f3.getSequenceFromMask(mask=('[#7f ]', ), )
    f4 = a.instances['Elektrode_unten'].faces
    faces4 = f4.getSequenceFromMask(mask=('[#1f ]', ), )
    a.Set(faces=faces1+faces2+faces3+faces4, name='NALL')
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Blech-1'].faces
    faces1 = f1.getSequenceFromMask(mask=('[#7f ]', ), )
    f2 = a.instances['Blech-2'].faces
    faces2 = f2.getSequenceFromMask(mask=('[#7f ]', ), )
    a.Set(faces=faces1+faces2, name='NALL_BLECH')
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Elektrode_oben'].faces
    faces1 = f1.getSequenceFromMask(mask=('[#1f ]', ), )
    a.Set(faces=faces1, name='NSET_E1')
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Elektrode_unten'].faces
    faces1 = f1.getSequenceFromMask(mask=('[#1f ]', ), )
    a.Set(faces=faces1, name='NSET_E2')
    session.viewports['Viewport: 1'].view.setValues(nearPlane=57.9758, 
        farPlane=65.5044, width=19.0767, height=20.5814, viewOffsetX=-3.3749, 
        viewOffsetY=-0.0739821)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Blech-2'].edges
    edges1 = e1.getSequenceFromMask(mask=('[#20000 ]', ), )
    a.Set(edges=edges1, name='NSET_TEMP')        
    session.viewports['Viewport: 1'].view.setValues(nearPlane=56.7152, 
        farPlane=66.765, width=25.4282, height=27.434, viewOffsetX=-4.40888, 
        viewOffsetY=0.374022)
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Elektrode_oben'].faces
    faces1 = f1.getSequenceFromMask(mask=('[#1f ]', ), )
    f2 = a.instances['Elektrode_unten'].faces
    faces2 = f2.getSequenceFromMask(mask=('[#1f ]', ), )
    a.Set(faces=faces1+faces2, name='SET-1')
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Blech-1'].faces
    faces1 = f1.getSequenceFromMask(mask=('[#7f ]', ), )
    a.Set(faces=faces1, name='SET-2')
    a = mdb.models['Model-1'].rootAssembly
    f1 = a.instances['Blech-2'].faces
    faces1 = f1.getSequenceFromMask(mask=('[#7f ]', ), )
    a.Set(faces=faces1, name='SET-3')
    a = mdb.models['Model-1'].rootAssembly
    v1 = a.instances['Elektrode_oben'].vertices
    verts1 = v1.getSequenceFromMask(mask=('[#4 ]', ), )
    a.Set(vertices=verts1, name='LOAD_ELEKTRODE_REFERENZ')
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_oben'].edges
    edges1 = e1.getSequenceFromMask(mask=('[#44 ]', ), )
    a.Set(edges=edges1, name='LOAD_ELEKTRODE')
    session.viewports['Viewport: 1'].view.setValues(nearPlane=56.5561, 
        farPlane=66.9241, width=26.229, height=28.2979, viewOffsetX=-4.88698, 
        viewOffsetY=1.17112)
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_oben'].edges
    edges1 = e1.getSequenceFromMask(mask=('[#44 ]', ), )
    a.Set(edges=edges1, name='LOAD_ELEKTRODE')
    a = mdb.models['Model-1'].rootAssembly
    e1 = a.instances['Elektrode_oben'].edges
    edges1 = e1.getSequenceFromMask(mask=('[#44 ]', ), )
    xv1 = a.instances['Elektrode_oben'].vertices
    xVerts1 = xv1.getSequenceFromMask(mask=('[#4 ]', ), )
    a.Set(edges=edges1, xVertices=xVerts1, name='LOAD_ELEKTRODE')
    session.viewports['Viewport: 1'].view.setValues(nearPlane=28.5175, 
        farPlane=35.6623, width=36.0642, height=17.6482, viewOffsetX=3.74818, 
        viewOffsetY=0.811604)
    a = mdb.models['Model-1'].rootAssembly
    v1 = a.instances['Blech-2'].vertices
    verts1 = v1.getSequenceFromMask(mask=('[#8 ]', ), )
    a.Set(vertices=verts1, name='BC_Blechfix')


def Macro_8_Interaktionen():
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import step
    import interaction
    import load
    import mesh
    import optimization
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
    mdb.models['Model-1'].ContactProperty('BLECH')
    mdb.models['Model-1'].interactionProperties['BLECH'].TangentialBehavior(
        formulation=PENALTY, directionality=ISOTROPIC, slipRateDependency=OFF, 
        pressureDependency=OFF, temperatureDependency=OFF, dependencies=0, 
        table=((0.1, ), ), shearStressLimit=None, maximumElasticSlip=FRACTION, 
        fraction=0.005, elasticSlipStiffness=None)
    mdb.models['Model-1'].interactionProperties['BLECH'].NormalBehavior(
        pressureOverclosure=HARD, allowSeparation=ON, 
        constraintEnforcementMethod=DEFAULT)
    mdb.models['Model-1'].interactionProperties['BLECH'].GeometricProperties(
        contactArea=1.0, padThickness=None)
    mdb.models['Model-1'].ContactProperty('BLECH_NOSEP')
    mdb.models['Model-1'].interactionProperties['BLECH_NOSEP'].TangentialBehavior(
        formulation=PENALTY, directionality=ISOTROPIC, slipRateDependency=OFF, 
        pressureDependency=OFF, temperatureDependency=OFF, dependencies=0, 
        table=((0.1, ), ), shearStressLimit=None, maximumElasticSlip=FRACTION, 
        fraction=0.005, elasticSlipStiffness=None)
    mdb.models['Model-1'].interactionProperties['BLECH_NOSEP'].NormalBehavior(
        pressureOverclosure=HARD, allowSeparation=OFF, 
        constraintEnforcementMethod=DEFAULT)
    mdb.models['Model-1'].interactionProperties['BLECH_NOSEP'].GeometricProperties(
        contactArea=1.0, padThickness=None)
    mdb.models['Model-1'].ContactProperty('ELEKTRODE')
    mdb.models['Model-1'].interactionProperties['ELEKTRODE'].TangentialBehavior(
        formulation=PENALTY, directionality=ISOTROPIC, slipRateDependency=OFF, 
        pressureDependency=OFF, temperatureDependency=OFF, dependencies=0, 
        table=((0.1, ), ), shearStressLimit=None, maximumElasticSlip=FRACTION, 
        fraction=0.005, elasticSlipStiffness=None)
    mdb.models['Model-1'].interactionProperties['ELEKTRODE'].NormalBehavior(
        pressureOverclosure=HARD, allowSeparation=ON, 
        constraintEnforcementMethod=DEFAULT)
    mdb.models['Model-1'].interactionProperties['ELEKTRODE'].GeometricProperties(
        contactArea=1.0, padThickness=None)
    mdb.models['Model-1'].ContactProperty('ELEK_NOSEP')
    mdb.models['Model-1'].interactionProperties['ELEK_NOSEP'].TangentialBehavior(
        formulation=PENALTY, directionality=ISOTROPIC, slipRateDependency=OFF, 
        pressureDependency=OFF, temperatureDependency=OFF, dependencies=0, 
        table=((0.1, ), ), shearStressLimit=None, maximumElasticSlip=FRACTION, 
        fraction=0.005, elasticSlipStiffness=None)
    mdb.models['Model-1'].interactionProperties['ELEK_NOSEP'].NormalBehavior(
        pressureOverclosure=HARD, allowSeparation=OFF, 
        constraintEnforcementMethod=DEFAULT)
    mdb.models['Model-1'].interactionProperties['ELEK_NOSEP'].GeometricProperties(
        contactArea=1.0, padThickness=None)
    a = mdb.models['Model-1'].rootAssembly
    region1=a.surfaces['SURF_BL1_BL2']
    a = mdb.models['Model-1'].rootAssembly
    region2=a.surfaces['SURF_BL2_BL1']
    mdb.models['Model-1'].SurfaceToSurfaceContactStd(name='BLECH_1', 
        createStepName='Initial', master=region1, slave=region2, sliding=SMALL, 
        thickness=ON, interactionProperty='BLECH', adjustMethod=NONE, 
        initialClearance=OMIT, datumAxis=None, clearanceRegion=None)
    a = mdb.models['Model-1'].rootAssembly
    region1=a.surfaces['SURF2_BL1_BL2']
    a = mdb.models['Model-1'].rootAssembly
    region2=a.surfaces['SURF2_BL2_BL1']
    mdb.models['Model-1'].SurfaceToSurfaceContactStd(name='BLECH_NOSEP_1', 
        createStepName='Initial', master=region1, slave=region2, sliding=SMALL, 
        thickness=ON, interactionProperty='BLECH_NOSEP', adjustMethod=NONE, 
        initialClearance=OMIT, datumAxis=None, clearanceRegion=None)
    mdb.models['Model-1'].interactions['BLECH_NOSEP_1'].deactivate('Step-1')
    mdb.models['Model-1'].interactions['BLECH_NOSEP_1'].reset('Step-1')
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-1')
    mdb.models['Model-1'].interactions['BLECH_NOSEP_1'].setValuesInStep(
        stepName='Step-1', activeInStep=False)
    a = mdb.models['Model-1'].rootAssembly
    region1=a.surfaces['SURF_EL1_BL1']
    a = mdb.models['Model-1'].rootAssembly
    region2=a.surfaces['SURF_BL1_EL1']
    mdb.models['Model-1'].SurfaceToSurfaceContactStd(name='ELEKTORDE_1', 
        createStepName='Step-1', master=region1, slave=region2, sliding=SMALL, 
        thickness=ON, interactionProperty='ELEKTRODE', adjustMethod=NONE, 
        initialClearance=OMIT, datumAxis=None, clearanceRegion=None)
    del mdb.models['Model-1'].interactions['ELEKTORDE_1']
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Initial')
    a = mdb.models['Model-1'].rootAssembly
    region1=a.surfaces['SURF_EL1_BL1']
    a = mdb.models['Model-1'].rootAssembly
    region2=a.surfaces['SURF_BL1_EL1']
    mdb.models['Model-1'].SurfaceToSurfaceContactStd(name='ELEKTRODE_1', 
        createStepName='Initial', master=region1, slave=region2, sliding=SMALL, 
        thickness=ON, interactionProperty='ELEKTRODE', adjustMethod=NONE, 
        initialClearance=OMIT, datumAxis=None, clearanceRegion=None)
    a = mdb.models['Model-1'].rootAssembly
    region1=a.surfaces['SURF_EL2_BL2']
    a = mdb.models['Model-1'].rootAssembly
    region2=a.surfaces['SURF_BL2_EL2']
    mdb.models['Model-1'].SurfaceToSurfaceContactStd(name='ELEKTRODE_2', 
        createStepName='Initial', master=region1, slave=region2, sliding=SMALL, 
        thickness=ON, interactionProperty='ELEKTRODE', adjustMethod=NONE, 
        initialClearance=OMIT, datumAxis=None, clearanceRegion=None)
    a = mdb.models['Model-1'].rootAssembly
    region1=a.surfaces['SURF2_EL1_BL1']
    a = mdb.models['Model-1'].rootAssembly
    region2=a.surfaces['SURF2_BL1_EL1']
    mdb.models['Model-1'].SurfaceToSurfaceContactStd(name='ELEK_NOSEP_1', 
        createStepName='Initial', master=region1, slave=region2, sliding=SMALL, 
        thickness=ON, interactionProperty='ELEK_NOSEP', adjustMethod=NONE, 
        initialClearance=OMIT, datumAxis=None, clearanceRegion=None)
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-1')
    mdb.models['Model-1'].interactions['ELEK_NOSEP_1'].setValuesInStep(
        stepName='Step-1', activeInStep=False)
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Initial')
    a = mdb.models['Model-1'].rootAssembly
    region1=a.surfaces['SURF2_EL2_BL2']
    a = mdb.models['Model-1'].rootAssembly
    region2=a.surfaces['SURF2_BL2_EL2']
    mdb.models['Model-1'].SurfaceToSurfaceContactStd(name='ELEK_NOSEP_2', 
        createStepName='Initial', master=region1, slave=region2, sliding=SMALL, 
        thickness=ON, interactionProperty='ELEK_NOSEP', adjustMethod=NONE, 
        initialClearance=OMIT, datumAxis=None, clearanceRegion=None)
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-1')
    mdb.models['Model-1'].interactions['ELEK_NOSEP_2'].setValuesInStep(
        stepName='Step-1', activeInStep=False)
    mdb.models['Model-1'].Equation(name='Eqn-1', terms=((-1.0, 'LOAD_ELEKTRODE', 
        2), (1.0, 'LOAD_ELEKTRODE_REFERENZ', 2)))
    # Gap Conductance hinzugefügt
    mdb.models['Model-1'].interactionProperties['BLECH'].tangentialBehavior.setValues(
        formulation=PENALTY, directionality=ISOTROPIC, slipRateDependency=OFF, 
        pressureDependency=OFF, temperatureDependency=OFF, dependencies=0, 
        table=((0.1, ), ), shearStressLimit=None, maximumElasticSlip=FRACTION, 
        fraction=0.005, elasticSlipStiffness=None)
    mdb.models['Model-1'].interactionProperties['BLECH'].normalBehavior.setValues(
        pressureOverclosure=HARD, allowSeparation=ON, 
        constraintEnforcementMethod=DEFAULT)
    mdb.models['Model-1'].interactionProperties['BLECH'].geometricProperties.setValues(
        contactArea=1.0, padThickness=None)
    mdb.models['Model-1'].interactionProperties['BLECH'].ThermalConductance(
        definition=TABULAR, clearanceDependency=ON, pressureDependency=OFF, 
        temperatureDependencyC=ON, massFlowRateDependencyC=OFF, 
        dependenciesC=0, clearanceDepTable=((0.39062, 0.0, 20.0), (0.0, 0.05, 
        20.0), (0.37042, 0.0, 93.0), (0.0, 0.05, 93.0), (0.35545, 0.0, 204.0), 
        (0.0, 0.05, 204.0), (0.34572, 0.0, 316.0), (0.0, 0.05, 316.0), (
        0.33525, 0.0, 427.0), (0.0, 0.05, 427.0), (0.32028, 0.0, 538.0), (0.0, 
        0.05, 538.0), (0.31579, 0.0, 649.0), (0.0, 0.05, 649.0), (0.31055, 0.0, 
        760.0), (0.0, 0.05, 760.0), (0.30532, 0.0, 871.0), (0.0, 0.05, 871.0), 
        (0.30083, 0.0, 982.0), (0.0, 0.05, 982.0)))
    mdb.models['Model-1'].interactionProperties['ELEKTRODE'].tangentialBehavior.setValues(
        formulation=PENALTY, directionality=ISOTROPIC, slipRateDependency=OFF, 
        pressureDependency=OFF, temperatureDependency=OFF, dependencies=0, 
        table=((0.1, ), ), shearStressLimit=None, maximumElasticSlip=FRACTION, 
        fraction=0.005, elasticSlipStiffness=None)
    mdb.models['Model-1'].interactionProperties['ELEKTRODE'].ThermalConductance(
        definition=TABULAR, clearanceDependency=ON, pressureDependency=OFF, 
        temperatureDependencyC=ON, massFlowRateDependencyC=OFF, 
        dependenciesC=0, clearanceDepTable=((50.0, 0.0, 20.0), (0.0, 0.05, 
        20.0), (130.0, 0.0, 200.0), (0.0, 0.05, 200.0), (200.0, 0.0, 300.0), (
        0.0, 0.05, 300.0), (670.0, 0.0, 400.0), (0.0, 0.05, 400.0), (1840.0, 
        0.0, 500.0), (0.0, 0.05, 500.0), (2560.0, 0.0, 600.0), (0.0, 0.05, 
        600.0)))


def Macro_9_Belastung():
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import step
    import interaction
    import load
    import mesh
    import optimization
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(loads=ON, bcs=ON, 
        predefinedFields=ON, interactions=OFF, constraints=OFF, 
        engineeringFeatures=OFF)
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-3')
    a = mdb.models['Model-1'].rootAssembly
    region = a.sets['LOAD_ELEKTRODE_REFERENZ']
    mdb.models['Model-1'].ConcentratedForce(name='CFORCE_1', 
        createStepName='Step-3', region=region, cf2=-3412.97, 
        distributionType=UNIFORM, field='', localCsys=None)
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Initial')
    a = mdb.models['Model-1'].rootAssembly
    region = a.sets['BC_ELEKTRODE']
    mdb.models['Model-1'].EncastreBC(name='DISP_BC_1', createStepName='Initial', 
        region=region, localCsys=None)
    mdb.models['Model-1'].boundaryConditions['DISP_BC_1'].deactivate('Step-1')
    a = mdb.models['Model-1'].rootAssembly
    region = a.sets['BC_TEMP']
    mdb.models['Model-1'].EncastreBC(name='DISP_BC_2', createStepName='Initial', 
        region=region, localCsys=None)
    mdb.models['Model-1'].boundaryConditions['DISP_BC_2'].deactivate('Step-2')
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-1')
    a = mdb.models['Model-1'].rootAssembly
    region = a.sets['BC_ELEKTRODE']
    mdb.models['Model-1'].EncastreBC(name='DISP_BC_3', createStepName='Step-1', 
        region=region, localCsys=None)
    mdb.models['Model-1'].boundaryConditions['DISP_BC_3'].deactivate('Step-2')
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-2')
    a = mdb.models['Model-1'].rootAssembly
    region = a.sets['BC_ELEKTRODE']
    mdb.models['Model-1'].EncastreBC(name='DISP_BC_4', createStepName='Step-2', 
        region=region, localCsys=None)
    mdb.models['Model-1'].boundaryConditions['DISP_BC_4'].deactivate('Step-3')
    a = mdb.models['Model-1'].rootAssembly
    region = a.sets['NSET_E1']
    mdb.models['Model-1'].DisplacementBC(name='DISP_BC_5', createStepName='Step-2', 
        region=region, u1=UNSET, u2=-0.001, ur3=UNSET, amplitude=UNSET, 
        fixed=OFF, distributionType=UNIFORM, fieldName='', localCsys=None)
    mdb.models['Model-1'].boundaryConditions['DISP_BC_5'].deactivate('Step-3')
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-3')
    a = mdb.models['Model-1'].rootAssembly
    region = a.sets['BC_ELEKTRODE']
    mdb.models['Model-1'].EncastreBC(name='DISP_BC_6', createStepName='Step-3', 
        region=region, localCsys=None)
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Initial')
    a = mdb.models['Model-1'].rootAssembly
    region = a.sets['NALL_BLECH']
    mdb.models['Model-1'].Temperature(name='FIELD_1', createStepName='Initial', 
        region=region, distributionType=UNIFORM, 
        crossSectionDistribution=CONSTANT_THROUGH_THICKNESS, magnitudes=(20.0, 
        ))
    

def Macro_2_Eigenschaften_Al6_HDI():
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import step
    import interaction
    import load
    import mesh
    import optimization
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
    session.viewports['Viewport: 1'].partDisplay.setValues(sectionAssignments=ON, 
        engineeringFeatures=ON)
    session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
        referenceRepresentation=OFF)
    mdb.models['Model-1'].Material(name='CuCrZr')
    mdb.models['Model-1'].materials['CuCrZr'].Elastic(temperatureDependency=ON, 
        table=((135000.0, 0.3, 20.0), ))
    mdb.models['Model-1'].materials['CuCrZr'].Conductivity(
        temperatureDependency=ON, table=((320.0, 20.0), ))
    mdb.models['Model-1'].materials['CuCrZr'].ElectricalConductivity(
        temperatureDependency=ON, table=((43500000.0, 20.0), ))
    mdb.models['Model-1'].materials['CuCrZr'].SpecificHeat(
        temperatureDependency=ON, table=((370000000.0, 20.0), ))
    mdb.models['Model-1'].materials['CuCrZr'].Density(table=((8.91e-09, ), ))
    mdb.models['Model-1'].materials['CuCrZr'].UserOutputVariables(n=6)
    mdb.models['Model-1'].Material(name='Al6_HDI')
    mdb.models['Model-1'].materials['Al6_HDI'].Conductivity(
        temperatureDependency=ON, table=((164.4, 34.5), (171.5, 50.0), (190.6, 
        100.0), (202.5, 150.0), (210.0, 200.0), (212.9, 250.0), (212.4, 300.0), 
        (210.2, 350.0), (207.9, 400.0), (203.9, 450.0), (196.2, 500.0)))
    mdb.models['Model-1'].materials['Al6_HDI'].Density(temperatureDependency=ON, 
        table=((2.7154e-09, 20.0), ))
    mdb.models['Model-1'].materials['Al6_HDI'].Depvar(n=10)
    mdb.models['Model-1'].materials['Al6_HDI'].Elastic(temperatureDependency=ON, 
        table=((68800.0, 0.34, 20.0), (65800.0, 0.34, 100.0), (60300.0, 0.34, 
        200.0), (52400.0, 0.34, 300.0), (42386.0, 0.34, 400.0), (30153.0, 0.34, 
        500.0)))
    mdb.models['Model-1'].materials['Al6_HDI'].ElectricalConductivity(
        temperatureDependency=ON, table=((26062553.0, 34.5), (24914883.0, 
        100.0), (21644767.0, 200.0), (18073992.0, 300.0), (15060985.0, 400.0), 
        (12376905.0, 500.0)))
    mdb.models['Model-1'].materials['Al6_HDI'].Expansion(table=((6.93e-06, 30.0), (
        1.534e-05, 50.0), (2.138e-05, 100.0), (2.367e-05, 200.0), (2.462e-05, 
        300.0), (2.557e-05, 400.0), (2.65e-05, 500.0)), 
        temperatureDependency=ON)
    mdb.models['Model-1'].materials['Al6_HDI'].LatentHeat(table=((
        390200000000.0, 612.0, 655.0), ))
    mdb.models['Model-1'].materials['Al6_HDI'].Plastic(temperatureDependency=ON, 
        table=((93.16, 0.0, 20.0), (117.87, 0.002, 20.0), (136.45, 0.01, 20.0), 
        (153.19, 0.0201, 20.0), (190.6, 0.05, 20.0), (227.51, 0.1, 20.0), (
        249.16, 0.1501, 20.0), (262.32, 0.2001, 20.0), (93.16, 0.0, 100.0), (
        134.87, 0.0102, 100.0), (156.77, 0.0253, 100.0), (183.97, 0.0501, 
        100.0), (214.91, 0.1001, 100.0), (233.18, 0.1498, 100.0), (247.81, 
        0.2346, 100.0), (61.96, 0.0, 200.0), (108.03, 0.0038, 200.0), (112.28, 
        0.006, 200.0), (133.55, 0.0209, 200.0), (156.8, 0.0515, 200.0), (
        172.62, 0.1011, 200.0), (177.44, 0.1219, 200.0), (182.5, 0.1513, 
        200.0), (184.91, 0.1907, 200.0), (61.96, 0.0, 300.0), (72.1, 0.005, 
        300.0), (76.62, 0.0111, 300.0), (79.86, 0.0201, 300.0), (86.65, 0.0517, 
        300.0), (92.35, 0.1014, 300.0), (96.04, 0.1515, 300.0), (98.55, 0.2014, 
        300.0), (41.39, 0.0, 400.0), (42.49, 0.0015, 400.0), (44.23, 0.0104, 
        400.0), (45.24, 0.0242, 400.0), (45.47, 0.0491, 400.0), (47.23, 0.2202, 
        400.0), (25.92, 0.0, 500.0), (26.25, 0.0052, 500.0), (26.96, 0.0151, 
        500.0), (28.03, 0.0494, 500.0), (28.93, 0.1007, 500.0), (31.11, 0.2574, 
        500.0), (5.0, 0.0, 600.0), (5.0, 5.0, 600.0)))
    mdb.models['Model-1'].materials['Al6_HDI'].plastic.AnnealTemperature(table=((
        635.0, ), ))
    mdb.models['Model-1'].materials['Al6_HDI'].SpecificHeat(
        temperatureDependency=ON, table=((895000000.0, 20.0), (914000000.0, 
        50.0), (942000000.0, 100.0), (968000000.0, 150.0), (994000000.0, 
        200.0), (1028000000.0, 250.0), (1058000000.0, 300.0), (1102000000.0, 
        350.0), (1157000000.0, 400.0), (1226000000.0, 450.0), (1276000000.0, 
        480.0), (1136000000.0, 490.0), (1146000000.0, 500.0), (1210000000.0, 
        550.0)))
    mdb.models['Model-1'].materials['Al6_HDI'].UserDefinedField()
    mdb.models['Model-1'].materials['Al6_HDI'].UserOutputVariables(n=6)
    mdb.models['Model-1'].HomogeneousSolidSection(name='Section-1', 
        material='CuCrZr', thickness=1.0)
    mdb.models['Model-1'].HomogeneousSolidSection(name='Section-2', 
        material='Al6_HDI', thickness=1.0)
    mdb.models['Model-1'].HomogeneousSolidSection(name='Section-3', 
        material='CuCrZr', thickness=1.0)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    session.viewports['Viewport: 1'].setValues(displayedObject=p)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=28.2053, 
        farPlane=34.2767, width=13.8052, height=16.557, viewOffsetX=0.761978, 
        viewOffsetY=-0.335184)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    f = p.faces
    faces = f.getSequenceFromMask(mask=('[#1f ]', ), )
    region = regionToolset.Region(faces=faces)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    p.SectionAssignment(region=region, sectionName='Section-1', offset=0.0, 
        offsetType=MIDDLE_SURFACE, offsetField='', 
        thicknessAssignment=FROM_SECTION)
    p = mdb.models['Model-1'].parts['Blech']
    session.viewports['Viewport: 1'].setValues(displayedObject=p)
    p = mdb.models['Model-1'].parts['Blech']
    f = p.faces
    faces = f.getSequenceFromMask(mask=('[#7f ]', ), )
    region = regionToolset.Region(faces=faces)
    p = mdb.models['Model-1'].parts['Blech']
    p.SectionAssignment(region=region, sectionName='Section-2', offset=0.0, 
        offsetType=MIDDLE_SURFACE, offsetField='', 
        thicknessAssignment=FROM_SECTION)
    p = mdb.models['Model-1'].parts['Elektrode_unten']
    session.viewports['Viewport: 1'].setValues(displayedObject=p)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=26.3659, 
        farPlane=36.1161, width=19.9817, height=23.9647, viewOffsetX=-1.00126, 
        viewOffsetY=1.43694)
    p = mdb.models['Model-1'].parts['Elektrode_unten']
    f = p.faces
    faces = f.getSequenceFromMask(mask=('[#1f ]', ), )
    region = regionToolset.Region(faces=faces)
    p = mdb.models['Model-1'].parts['Elektrode_unten']
    p.SectionAssignment(region=region, sectionName='Section-3', offset=0.0, 
        offsetType=MIDDLE_SURFACE, offsetField='', 
        thicknessAssignment=FROM_SECTION)



def Macro_2_Eigenschaften_Al5_STD():
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import step
    import interaction
    import load
    import mesh
    import optimization
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
    session.viewports['Viewport: 1'].partDisplay.setValues(sectionAssignments=ON, 
        engineeringFeatures=ON)
    session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
        referenceRepresentation=OFF)
    mdb.models['Model-1'].Material(name='CuCrZr')
    mdb.models['Model-1'].materials['CuCrZr'].Elastic(temperatureDependency=ON, 
        table=((135000.0, 0.3, 20.0), ))
    mdb.models['Model-1'].materials['CuCrZr'].Conductivity(
        temperatureDependency=ON, table=((320.0, 20.0), ))
    mdb.models['Model-1'].materials['CuCrZr'].ElectricalConductivity(
        temperatureDependency=ON, table=((43500000.0, 20.0), ))
    mdb.models['Model-1'].materials['CuCrZr'].SpecificHeat(
        temperatureDependency=ON, table=((370000000.0, 20.0), ))
    mdb.models['Model-1'].materials['CuCrZr'].Density(table=((8.91e-09, ), ))
    mdb.models['Model-1'].materials['CuCrZr'].UserOutputVariables(n=6)
    mdb.models['Model-1'].Material(name='Al5_STD')
    mdb.models['Model-1'].materials['Al5_STD'].Conductivity(
        temperatureDependency=ON, table=((106.3, 20.0), (122.3, 100.0), (136.2, 
        200.0), (142.1, 300.0), (144.4, 400.0), (142.1, 500.0)))
    mdb.models['Model-1'].materials['Al5_STD'].Density(temperatureDependency=ON, 
        table=((2.6834e-09, 20.0), ))
    mdb.models['Model-1'].materials['Al5_STD'].Depvar(n=10)
    mdb.models['Model-1'].materials['Al5_STD'].Elastic(temperatureDependency=ON, 
        table=((71300.0, 0.34, 20.0), (70300.0, 0.34, 50.0), (68900.0, 0.34, 
        100.0), (67200.0, 0.34, 150.0), (65100.0, 0.34, 200.0), (62700.0, 0.34, 
        250.0), (59800.0, 0.34, 300.0), (56600.0, 0.34, 350.0), (53000.0, 0.34, 
        400.0), (44950.0, 0.34, 500.0)))
    mdb.models['Model-1'].materials['Al5_STD'].ElectricalConductivity(
        temperatureDependency=ON, table=((16920000.0, 20.0), (15980000.0, 
        100.0), (14040000.0, 200.0), (12090000.0, 300.0), (10460000.0, 400.0), 
        (9074000.0, 500.0)))
    mdb.models['Model-1'].materials['Al5_STD'].Expansion(table=((9.53e-06, 20.0), (
        1.708e-05, 50.0), (2.231e-05, 100.0), (2.448e-05, 200.0), (2.57e-05, 
        300.0), (2.633e-05, 400.0), (2.62e-05, 500.0)), 
        temperatureDependency=ON)
    mdb.models['Model-1'].materials['Al5_STD'].LatentHeat(table=((
        361000000000.0, 568.0, 634.0), ))
    mdb.models['Model-1'].materials['Al5_STD'].Plastic(temperatureDependency=ON, 
        table=((136.14, 0.0, 20.0), (158.24, 0.01, 20.0), (180.43, 0.0201, 
        20.0), (232.77, 0.05, 20.0), (284.7, 0.1, 20.0), (320.74, 0.1501, 
        20.0), (347.05, 0.2088, 20.0), (136.14, 0.0, 100.0), (158.24, 0.01, 
        100.0), (180.43, 0.0201, 100.0), (232.77, 0.05, 100.0), (281.9, 0.1027, 
        100.0), (307.7, 0.15, 100.0), (332.53, 0.2103, 100.0), (81.21, 0.0, 
        200.0), (123.75, 0.0022, 200.0), (149.37, 0.0101, 200.0), (167.8, 
        0.0202, 200.0), (201.73, 0.0502, 200.0), (232.09, 0.1001, 200.0), (
        253.66, 0.1505, 200.0), (268.26, 0.202, 200.0), (278.83, 0.2712, 
        200.0), (81.21, 0.0, 300.0), (109.7, 0.005, 300.0), (119.4, 0.0107, 
        300.0), (129.0, 0.02, 300.0), (145.04, 0.0501, 300.0), (156.19, 0.1006, 
        300.0), (161.7, 0.1516, 300.0), (163.4, 0.1781, 300.0), (26.7, 0.0, 
        400.0), (58.26, 0.002, 400.0), (72.19, 0.0055, 400.0), (84.23, 0.0113, 
        400.0), (89.02, 0.0206, 400.0), (89.23, 0.0506, 400.0), (26.7, 0.0, 
        500.0), (35.34, 0.001, 500.0), (39.84, 0.0055, 500.0), (40.79, 0.0101, 
        500.0), (41.26, 0.0213, 500.0), (42.0, 0.0585, 500.0)))
    mdb.models['Model-1'].materials['Al5_STD'].plastic.AnnealTemperature(table=((
        600.0, ), ))
    mdb.models['Model-1'].materials['Al5_STD'].SpecificHeat(
        temperatureDependency=ON, table=((932000000.0, 20.0), (963000000.0, 
        50.0), (1013000000.0, 100.0), (1066000000.0, 150.0), (1125000000.0, 
        200.0), (1180000000.0, 240.0), (1020000000.0, 250.0), (1043000000.0, 
        300.0), (1057000000.0, 350.0), (1082000000.0, 400.0), (1112000000.0, 
        450.0), (1157000000.0, 500.0), (1232000000.0, 550.0)))
    mdb.models['Model-1'].materials['Al5_STD'].UserDefinedField()
    mdb.models['Model-1'].materials['Al5_STD'].UserOutputVariables(n=6)
    mdb.models['Model-1'].HomogeneousSolidSection(name='Section-1', 
        material='CuCrZr', thickness=1.0)
    mdb.models['Model-1'].HomogeneousSolidSection(name='Section-2', 
        material='Al5_STD', thickness=1.0)
    mdb.models['Model-1'].HomogeneousSolidSection(name='Section-3', 
        material='CuCrZr', thickness=1.0)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    session.viewports['Viewport: 1'].setValues(displayedObject=p)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=28.2053, 
        farPlane=34.2767, width=13.8052, height=16.557, viewOffsetX=0.761978, 
        viewOffsetY=-0.335184)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    f = p.faces
    faces = f.getSequenceFromMask(mask=('[#1f ]', ), )
    region = regionToolset.Region(faces=faces)
    p = mdb.models['Model-1'].parts['Elektrode_oben']
    p.SectionAssignment(region=region, sectionName='Section-1', offset=0.0, 
        offsetType=MIDDLE_SURFACE, offsetField='', 
        thicknessAssignment=FROM_SECTION)
    p = mdb.models['Model-1'].parts['Blech']
    session.viewports['Viewport: 1'].setValues(displayedObject=p)
    p = mdb.models['Model-1'].parts['Blech']
    f = p.faces
    faces = f.getSequenceFromMask(mask=('[#7f ]', ), )
    region = regionToolset.Region(faces=faces)
    p = mdb.models['Model-1'].parts['Blech']
    p.SectionAssignment(region=region, sectionName='Section-2', offset=0.0, 
        offsetType=MIDDLE_SURFACE, offsetField='', 
        thicknessAssignment=FROM_SECTION)
    p = mdb.models['Model-1'].parts['Elektrode_unten']
    session.viewports['Viewport: 1'].setValues(displayedObject=p)
    session.viewports['Viewport: 1'].view.setValues(nearPlane=26.3659, 
        farPlane=36.1161, width=19.9817, height=23.9647, viewOffsetX=-1.00126, 
        viewOffsetY=1.43694)
    p = mdb.models['Model-1'].parts['Elektrode_unten']
    f = p.faces
    faces = f.getSequenceFromMask(mask=('[#1f ]', ), )
    region = regionToolset.Region(faces=faces)
    p = mdb.models['Model-1'].parts['Elektrode_unten']
    p.SectionAssignment(region=region, sectionName='Section-3', offset=0.0, 
        offsetType=MIDDLE_SURFACE, offsetField='', 
        thicknessAssignment=FROM_SECTION)

def Macro10_job_und_inp():
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import step
    import interaction
    import load
    import mesh
    import optimization
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
    mdb.Job(name='Job-1', model='Model-1', description='', type=ANALYSIS, 
        atTime=None, waitMinutes=0, waitHours=0, queue=None, memory=90, 
        memoryUnits=PERCENTAGE, getMemoryFromAnalysis=True, 
        explicitPrecision=SINGLE, nodalOutputPrecision=SINGLE, echoPrint=OFF, 
        modelPrint=OFF, contactPrint=OFF, historyPrint=OFF, userSubroutine='', 
        scratch='', resultsFormat=ODB, multiprocessingMode=DEFAULT, numCpus=1, 
        numGPUs=0)
    mdb.jobs['Job-1'].writeInput(consistencyChecking=OFF)

if __name__=='__main__':
    Macro_1_Geometrie()
    Macro_2_Eigenschaften_Al5182()
    Macro_2_Eigenschaften_Al6_HDI()
    Macro_2_Eigenschaften_Al5_STD()
    Macro_3_Zusammenbau()
    Macro_4_Oberflaechen()
    Macro_5_Schritte()
    Macro_6_Vernetzung()
    Macro_7_Sets()
    Macro_8_Interaktionen()
    Macro_9_Belastung()
    mdb.saveAs(pathName='Model.cae')
    Macro10_job_und_inp()