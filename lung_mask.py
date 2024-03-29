# -*- coding: utf-8 -*-
"""lung_mask.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1prfYjGHt9AK1_0ip7siipp_5lxUSE9Nd
"""

import io
import numpy as np
import pydicom
from pydicom.uid import generate_uid

class point(object):
  def __init__(self,x,y):
    self.x = x
    self.y = y

  def get_x(self):
    return self.x
  
  def get_y(self):
    return self.y

  def select_neighbors(p):
    if p != 0:
      neighbors = [point(-1,-1), point(0,-1), point (1,-1), point(1,0), point(1,1), point(0,1), point (-1,1), point (-1,0)]

    else:
      neighbors = [point(0,-1), point(1,0), point(0,1), point(-1,0)]
    return neighbors
  
  def get_gray_diff(bin_img,current_point,tmp_point):
    return abs(int(img[current_point.x.current_point.y]) - int(img[tmp_point.x,tmp_point.y]))
  
  def region_grow(bin_img,seeds,p=1):
    height, width = bin_img.shape
    seed_m = np.zeroes(bin_img.shape)
    seeds = []
    to_return = []
    for seed in seeds:
      seeds.append(seed)
    label = 1
    neighbors = select_neighbors(p)
    while(len(seeds) > 0):
      current_point = seedList.pop(0)
      seed_mark[current_point.x,current_point.y] = label
      for i in range(8):
        tmp_x = current_point.x + neighbors[i].x
        tmp_y = current_point.y + neighbors[i].y
        if tmp_x < 0 or tmp_y < 0 or tmp_x >= width or tmp_y >= height:
          continue
        gray_diff = get_gray_diff(bin_img, current_point, point(tmp_x,tmp_y))
        if gray_diff == 0 and seed_m[tmp_x,tmp_y] == 0:
          seed_m[tmp_x,tmp_y] = label
          seeds.append(Point(tmp_x,tmp_y))
          to_return.append((tmp_x,tmp_y))
    return to_return

def process_file(input_file, output_file, thresh_val, set_hu_val, new_series_uid, series_number):
  dcm = pydicom.dcmread(input_file)

  slope = float(dcm.RescaleSlope)
  intercept = float(dcm.RescaleIntercept)

  pixel_array = dcm.pixel_array
  if len(pixel_array.shape) == 3:
    pixel_array = pixel_array[:,:,0]

  pixel_array += gap 
  intercept -= slope*gap 
  hu_img = slope * pixel_array + intercept 
  bin_img = np.array(hu_img >= -700,dtype=np.int8)

  masked_img = hu_img[bin_img == region_grow(bin_img,seeds)] = -10000

  dcm.RescaleIntercept = intercept
  dcm.PixelData = dcm.pixel_array.tobytes()
  dcm.SeriesInstanceUID = new_series_uid
  dcm.SOPInstanceUID = generate_uid()
  dcm.SeriesDescription = getattr(dcm, 'SeriesDescription','') + ' (voxel manipulated)'
  dcm.ImageType = 'DERIVED/SECONDARY'
  dcm.SeriesNumber = series_number + 1
  dcm.BitsStored = dcm.BitsAllocated

  dcm.save_as(output_file)

def process_image(api,study,image,new_series_uid, series_number):
  url_template = "/study/{namespace}/{study_uid}/image/{image_uid}/version/{image_version}?pretranscode=1"
  furl = api.Storage.format_url(
      url_template,
      namespace=study.storage_namespace,
      study_uid=study.study_uid,
      image_uid=image.id,
      image_version=image.version,
      engine_fdqn=study.engine_fdqn
  )
  resp = api.Storage.get(furl, params={},stream=True)
  content = io.BytesIO(resp.content)
  corrected_file = io.BytesIO()
  process_file(content, corrected_file, generated_series_uid, sngle_series.get('id'))
  corrected_file.seek(0)
  api.Storage.Image.upload(
      engine_fdqn=study.engine_fqdn,
      namespace=study.storage_namespace,
      opened_file=corrected_file
  )

def process_study(api, study_uuid):
  study = api.Study.get(uuid=study_uuid).get()
  schema = api.Storage.Study.schema(
      engine_fqdn=study.engine_fqdn,
      namespace=study.storage_namespace,
      study_uid=study.study_uid
  )
  from multiprocessing.pool import ThreadPool
  pool = ThreadPool(processes=4)

  for single_series in schema.series:
    generated_sereis_uid = generate_uid()
    for image in single_series.images:
      pool.apply_async(process_image, args=(
          api, study, image, generated_sereis_uid, single_series))
  pool.close()
  pool.join()

def custom_code(data):
  from platformAPI import create_ambra_sdk_api
  import platformAPI as papi
  api = create_ambra_sdk_api()
  process_study(api,data['study']['uuid'])
  return 'OK'

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--url')
    parser.add_argument('--username')
    parser.add_argument('--password')
    parser.add_argument('--study_uuid')
#     parser.add_argument('--threshold', default=150)
#     parser.add_argument('--set_hu_val', default=-10000)
    args = parser.parse_args()
    from ambra_sdk.api import Api
    api = Api.with_creds(args.url, args.username, args.password)
    from time import time
    start = time()
    process_study(api, args.study_uuid)
    print('Total time: {:0.1f}'.format(time() - start))

