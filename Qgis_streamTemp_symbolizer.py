from PyQt4.QtGui import QColor
from qgis.core import QgsSymbolV2, QgsRendererRangeV2, QgsGraduatedSymbolRendererV2

class VectorSymbolizer():

    symbology = "StreamTemp_Vector"

    def symbolize(self):
        # define ranges: label, lower value, upper value, color name
        cond_cat = (
            ('0-10', 0.0, 10.0, '"#0024E3'),
            ('10-12', 10.0, 12.0, '#0087CD'),
            ('12-14', 12.0, 14.0, '#16F45A'),
            ('14-16', 14.0, 16.0, '#73FF1A'),
            ('16-18', 16.0, 18.0, '#BDFF0C'),
            ('18-20', 18.0, 20.0, '#FFDD00'),
            ('20-22', 20.0, 22.0, '#FF9000'),
            ('22-24', 22.0, 24.0, '#FF4400'),
            ('24-26', 24.0, 26.0, '#FF1D00'),
            ('26-28', 26.0, 28.0, 'F70000'),
            ('>28', 28.0, 40.0, '#AA0000')
        )

        # create a category for each item in animals
        ranges = []
        for label, lower, upper, color in cond_cat:
            symbol = QgsSymbolV2.defaultSymbol(self.layer.geometryType())
            symbol.setColor(QColor(color))
            symbol.setWidth(0.5)
            rng = QgsRendererRangeV2(lower, upper, symbol, label)
            ranges.append(rng)

        # create the renderer and assign it to a layer
        expression = 'area_solar'  # field name
        self.renderer = QgsGraduatedSymbolRendererV2(expression, ranges)