
# coding: utf-8

# # UPS MONITORING
# Welcome to the object detection inference walkthrough!  This notebook will walk you step by step through the process of using a pre-trained model to detect objects in an image. Make sure to follow the [installation instructions](https://github.com/tensorflow/models/blob/master/research/object_detection/g3doc/installation.md) before you start.

# # Imports

# In[2]:


import numpy as np
import os
import six.moves.urllib as urllib
import sys
import tarfile
import tensorflow as tf

from collections import defaultdict
from io import StringIO
#from matplotlib import pyplot as plt
from PIL import Image

# This is needed since the notebook is stored in the object_detection folder.
sys.path.append("..")
#from object_detection.utils import ops as utils_ops

###INSERT CODE FOR PICAMERA
#import argparse
#import cv2
 
#ap = argparse.ArgumentParser()
#ap.add_argument("-i", "--image", required = True, help = "Path to the image")
#args = vars(ap.parse_args())
#image = cv2.imread(args["image"])
filename= "camera.jpg"

# ## Object detection imports
# Here are the imports from the object detection module.

from utils import label_map_util

from utils import visualization_utils as vis_util


# # Model preparation 

MODEL_NAME = 'ups_models'
# Path to frozen detection graph. This is the actual model that is used for the object detection.
PATH_TO_CKPT = MODEL_NAME + '/frozen_inference_graph.pb'

# List of the strings that is used to add correct label for each box.
PATH_TO_LABELS = os.path.join('data', 'UPS-model-classes.pbtxt')

NUM_CLASSES = 1

# ## Load a (frozen) Tensorflow model into memory.
detection_graph = tf.Graph()
with detection_graph.as_default():
  od_graph_def = tf.GraphDef()
  with tf.gfile.GFile(PATH_TO_CKPT, 'rb') as fid:
    serialized_graph = fid.read()
    od_graph_def.ParseFromString(serialized_graph)
    tf.import_graph_def(od_graph_def, name='')


# ## Loading label map
# Label maps map indices to category names, so that when our convolution network predicts `5`, we know that this corresponds to `airplane`.  Here we use internal utility functions, but anything that returns a dictionary mapping integers to appropriate string labels would be fine

label_map = label_map_util.load_labelmap(PATH_TO_LABELS)
categories = label_map_util.convert_label_map_to_categories(label_map, max_num_classes=NUM_CLASSES, use_display_name=True)
category_index = label_map_util.create_category_index(categories)


# ## Helper code

def load_image_into_numpy_array(image):
  (im_width, im_height) = image.size
  return np.array(image.getdata()).reshape(
      (im_height, im_width, 3)).astype(np.uint8)


# Detection
# If you want to test the code with your images, just add path to the images to the TEST_IMAGE_PATHS.

PATH_TO_TEST_IMAGES_DIR = 'images'
#TEST_IMAGE_PATHS = [ os.path.join(PATH_TO_TEST_IMAGES_DIR, 'cam{}.jpg'.format(i)) for i in range(60,66) ]
#TEST_IMAGE_PATHS = [ os.path.join(PATH_TO_TEST_IMAGES_DIR, 'cam{}.jpg'.format(2)) ]
TEST_IMAGE_PATHS = [ os.path.join(PATH_TO_TEST_IMAGES_DIR, filename) ]

# Size, in inches, of the output images.
IMAGE_SIZE = (12, 8)


def run_inference_for_single_image(image, graph):
  with graph.as_default():
    with tf.Session() as sess:
      # Get handles to input and output tensors
      ops = tf.get_default_graph().get_operations()
      all_tensor_names = {output.name for op in ops for output in op.outputs}
      tensor_dict = {}
      for key in [
          'num_detections', 'detection_boxes', 'detection_scores',
          'detection_classes', 'detection_masks'
      ]:
        tensor_name = key + ':0'
        if tensor_name in all_tensor_names:
          tensor_dict[key] = tf.get_default_graph().get_tensor_by_name(
              tensor_name)
      if 'detection_masks' in tensor_dict:
        # The following processing is only for single image
        detection_boxes = tf.squeeze(tensor_dict['detection_boxes'], [0])
        detection_masks = tf.squeeze(tensor_dict['detection_masks'], [0])
        # Reframe is required to translate mask from box coordinates to image coordinates and fit the image size.
        real_num_detection = tf.cast(tensor_dict['num_detections'][0], tf.int32)
        detection_boxes = tf.slice(detection_boxes, [0, 0], [real_num_detection, -1])
        detection_masks = tf.slice(detection_masks, [0, 0, 0], [real_num_detection, -1, -1])
        detection_masks_reframed = utils_ops.reframe_box_masks_to_image_masks(
            detection_masks, detection_boxes, image.shape[0], image.shape[1])
        detection_masks_reframed = tf.cast(
            tf.greater(detection_masks_reframed, 0.5), tf.uint8)
        # Follow the convention by adding back the batch dimension
        tensor_dict['detection_masks'] = tf.expand_dims(
            detection_masks_reframed, 0)
         
      image_tensor = tf.get_default_graph().get_tensor_by_name('image_tensor:0')

      # Run inference
      output_dict = sess.run(tensor_dict,
                             feed_dict={image_tensor: np.expand_dims(image, 0)})
    

      # all outputs are float32 numpy arrays, so convert types as appropriate
      output_dict['num_detections'] = int(output_dict['num_detections'][0])
      output_dict['detection_classes'] = output_dict[
          'detection_classes'][0].astype(np.uint8)
      output_dict['detection_boxes'] = output_dict['detection_boxes'][0]
      output_dict['detection_scores'] = output_dict['detection_scores'][0]
      if 'detection_masks' in output_dict:
        output_dict['detection_masks'] = output_dict['detection_masks'][0]
  return output_dict


# In[15]:


for image_path in TEST_IMAGE_PATHS:
  image = Image.open(image_path)
  # the array based representation of the image will be used later in order to prepare the
  # result image with boxes and labels on it.
  image_np = load_image_into_numpy_array(image)
  # Expand dimensions since the model expects images to have shape: [1, None, None, 3]
  image_np_expanded = np.expand_dims(image_np, axis=0)
  # Actual detection.
  output_dict = run_inference_for_single_image(image_np, detection_graph)
  # Visualization of the results of a detection.
  vis_util.visualize_boxes_and_labels_on_image_array(
      image_np,
      output_dict['detection_boxes'],
      output_dict['detection_classes'],
      output_dict['detection_scores'],
      category_index,
      instance_masks=output_dict.get('detection_masks'),
      use_normalized_coordinates=True,
      line_thickness=8)
#  plt.figure(figsize=IMAGE_SIZE)
#  plt.imshow(image_np)

import cv2
#img = cv2.imread(os.path.join(PATH_TO_TEST_IMAGES_DIR, filename))
width, height = image.size
ymin = output_dict['detection_boxes'][0,0]
xmin = output_dict['detection_boxes'][0,1]
ymax = output_dict['detection_boxes'][0,2]
xmax = output_dict['detection_boxes'][0,3]
(xminn, xmaxx, yminn, ymaxx) = (int(xmin * width), int(xmax * width), int(ymin * height), int(ymax * height)) 
cropped1 = image_np[yminn: (ymaxx), xminn:(xmaxx)]

lower_green = np.array([70, 100, 100])
upper_green = np.array([170, 255, 255])
hsv = cv2.cvtColor(cropped1, cv2.COLOR_BGR2HSV)
# Here we are defining range of bluecolor in HSV
# This creates a mask of green coloured
# objects found in the frame.
mask = cv2.inRange(hsv, lower_green, upper_green)

# The bitwise and of the frame and mask is done so
# that only the green coloured objects are highlighted
# and stored in res
res = cv2.bitwise_and(cropped1, cropped1, mask=mask)
green_count = (np.count_nonzero(res))
print (green_count)

lower_yellow = np.array([90, 255, 255])
upper_yellow = np.array([100, 255, 255])
mask = cv2.inRange(hsv, lower_yellow, upper_yellow)

res = cv2.bitwise_and(cropped1, cropped1, mask=mask)
yellow_count = (np.count_nonzero(res))
print (yellow_count)
lower_red = np.array([0, 60, 100])
upper_red = np.array([20, 100, 100])
mask = cv2.inRange(hsv, lower_red, upper_red)

res = cv2.bitwise_and(cropped1, cropped1, mask=mask)
red_count = (np.count_nonzero(res))
print (red_count)
green, yellow, red = "false", "false", "false"
if yellow_count > 0:
    yellow = "true"
elif green_count > 0:
    green = "true"
else:
    red = 'true'

print(red, yellow, green)
import csv

text = [filename, green, yellow, red]
print (text)
with open('colorcode.csv', 'a') as csvfile:
    colorcode = csv.writer(csvfile, delimiter=',', dialect='excel')
    colorcode.writerow(text)
