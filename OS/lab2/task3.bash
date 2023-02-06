#!/bin/bash
ps uax --sort=start_time | tail -n1 | awk '{print $2}'
