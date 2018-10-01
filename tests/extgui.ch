/*
 *  extGUI - GUI framework for Harbour
 *  extGUI is a Harbour library to build GUI application, using external
 *       standalone GUI server application.
 *  extGUI header file
 *
 * Copyright 2018 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define ANCHOR_TOPLEFT         0   // Anchors control to the top and left borders of the container and does not change the distance between the top and left borders. (Default)
#define ANCHOR_TOPABS          1   // Anchors control to top border of container and does not change the distance between the top border.
#define ANCHOR_LEFTABS         2   // Anchors control to left border of container and does not change the distance between the left border.
#define ANCHOR_BOTTOMABS       4   // Anchors control to bottom border of container and does not change the distance between the bottom border.
#define ANCHOR_RIGHTABS        8   // Anchors control to right border of container and does not change the distance between the right border.
#define ANCHOR_TOPREL          16  // Anchors control to top border of container and maintains relative distance between the top border.
#define ANCHOR_LEFTREL         32  // Anchors control to left border of container and maintains relative distance between the left border.
#define ANCHOR_BOTTOMREL       64  // Anchors control to bottom border of container and maintains relative distance between the bottom border.
#define ANCHOR_RIGHTREL        128 // Anchors control to right border of container and maintains relative distance between the right border.
#define ANCHOR_HORFIX          256 // Anchors center of control relative to left and right borders but remains fixed in size.
#define ANCHOR_VERTFIX         512 // Anchors center of control relative to top and bottom borders but remains fixed in size.
